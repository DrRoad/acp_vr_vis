################################################################################

### Prerequisites

################################################################################

# devtools::install_github("mdsumner/guerrilla")

library(guerrilla)
library(sf)
library(raster)
library(rgl)
library(tidyverse)
library(r2vr)
library(dismo)
library(fs)
library(r2vr)

source("./helpers/contour_to_raster.R")
source("./helpers/sf_to_trimesh.R")
source("./helpers/trimesh_to_threejson.R")

## Constants
MESH_FOOTPRINT_SCALE <- 0.1
MESH_HEIGHT_SCALE <- 3
################################################################################

### Read data

################################################################################

## read Alexander Clark Park

acp_contours <- sf::read_sf("data/alexander_clark_park/QSC_Extracted_Data_20171212_140035727000-20064/Contours_1_metre.shp")

## Project to metres
## Looked up Brisbane here to get UTM zone:
## https://www.latlong.net/place/brisbane-qld-australia-3337.html
##
acp_contours <-
  st_transform(acp_contours, crs = "+proj=utm +zone=56 +south +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Work out dimensions of plot
xdim <- st_bbox(acp_contours)$xmax - st_bbox(acp_contours)$xmin
ydim <- st_bbox(acp_contours)$ymax - st_bbox(acp_contours)$ymin

xdim
ydim

## If we want a 1m grid then we need aroppox 91 x 95 cells

acp_elev_raster <-
  contour_2_raster(acp_contours, 
                   n_samples = 100000,
                   raster_attribute = "ELEVATION",
                   ncol = 95,
                   nrow = 91)

## plot(acp_elev_raster)
## Looks good although a missing data issue due to data rotation.


## Create an extent to triangulate
## Just tucking in from each edge a little.
acp_bbox <- st_bbox(
  st_bbox(acp_elev_raster)[c(1, 3, 2, 4)] + c(1, -1, 1, -1) * 0.0005) %>%
  st_as_sfc() %>%
  st_multipolygon() %>%
  st_geometry()

## Set its spatial meta data
st_crs(acp_bbox) <- st_crs(acp_contours)


## Read in survey locations

survey_data <- readr::read_csv("./data/alexander_clark_park/points/koala-survey-sightings-data.csv")

sightings <-
  survey_data %>%
  filter(SiteName == "Alexander Clark Park") %>%
  select(Eastings, Northings)

## Assuming epsg 28356
## make sf points

point_geometry <-
  st_multipoint(as.matrix(sightings)) %>%
  st_geometry()

st_crs(point_geometry) <- st_crs("+init=epsg:28356")

## transform to lat lon to check
## 
 latlong_sighting <-
  st_transform(point_geometry, st_crs("+proj=longlat +ellps=WGS84"))

 library(leaflet)
 leaflet(data = (as(st_sf(st_cast(latlong_sighting, "POINT")), 'Spatial'))) %>%
  addTiles() %>%
  addMarkers()

## Looks good!


## Need to convert to mesh coords
## transform to mesh crs
point_geometry <- st_transform(point_geometry, st_crs(acp_contours))

sighting_matrix <- as(point_geometry[[1]], "matrix")
sighting_matrix <-
  cbind(sighting_matrix,
        raster::extract(acp_elev_raster, sighting_matrix[,1:2]) * MESH_HEIGHT_SCALE)
colnames(sighting_matrix) <- c("x","y","z")


################################################################################

## Mesh construction

################################################################################

## make a trimesh
acp_trimesh <- sf_to_trimesh(acp_bbox, 12000)

## add the height
acp_bbox_elev <-  raster::extract(acp_elev_raster,
                                     acp_trimesh$P[, 1:2])

## Some NAs set to min (1)
acp_bbox_elev[is.na(acp_bbox_elev)] <- min(acp_contours$ELEVATION)

acp_trimesh$P <- cbind(acp_trimesh$P, acp_bbox_elev * MESH_HEIGHT_SCALE)

ground_height <- min(acp_contours$ELEVATION)

height_correction <- -1 * (ground_height - mean(acp_trimesh$P[, 3]))
## We're reversing the correction that would have been applied to the
## ground height by centering.

## bind satellite tile

## there are better ways to get imagery, but this is a good old faithful detault
im <- dismo::gmap(acp_elev_raster,
                  type = "satellite", scale = 2)

## so this is a bit head-stretching, because multiple coordinate systems in play

## we want the [0,1,0,1] coordinates of the image in terms of the geographic mesh
## 1) mesh is lcc, backwards to longlat WGS84
xyl <- rgdal::project(acp_trimesh$P[, 1:2],
                      projection(acp_elev_raster), inv = TRUE)

## 2) image is merc, forwards to that
xym <- rgdal::project(xyl, projection(im))

## 3) the cell (in [0,1,0,1]) for our mesh coordinates
cell <- cellFromXY(im, xym)

## 4) use that cell to get native from a rescaled im
xyim <- xyFromCell(setExtent(im,
                             extent(0, 1, 0, 1)), cell)

## 5) convert to RGB from palette (might be a raster fun for this...)
imrgb <- setValues(brick(im, im, im),
                   t(col2rgb(im@legend@colortable[values(im) + 1])))

## 6) write to PNG (that's the only way we can texture map)
texfile <- "./data/acp_satellite.png"
rgdal::writeGDAL(as(imrgb, "SpatialGridDataFrame"),
                 texfile, driver = "PNG")

## generate JSON containing texture
## pass just the name of the texture file so it will look in the same directory
 acp_tex_json <-
   trimesh_to_threejson(vertices = acp_trimesh$P,
                       face_vertices = acp_trimesh$T,
                       vertex_uvs = xyim,
                       texture_file = fs::path_file(texfile)
                       )

acp_asset <- a_in_mem_asset(data = list(acp_tex_json, readr::read_file_raw(texfile)),
                            id = "acp",
                            src = "./data/acp.json",
                            parts = texfile)

################################################################################

## Scene construction

################################################################################


## Helper assets
## xy: an xy coordinate
a_roaming_koala <- function(xyz, radius = 1.5, n_paths = 3){
  
  koala_asset <- a_asset("koala", src = "./data/Mesh_Koala.gltf",
                         parts = c("./data/Mesh_Koala.bin",
                                   "./data/Tex_Koala.png")) 

  koala <- a_entity(gltf_model = koala_asset,
                    position = c(0, 2, 0),
                    scale = c(1, 1, 1) * 0.05,
                    alongpath = list(curve = "#track1",
                                     loop = TRUE,
                                     rotate = TRUE,
                                     dur = 3000))
  theta <- runif(n_paths)
  x <- cos(theta) * radius
  y <- sin(theta) * radius

  pmap(list(x,y), function(x,y){
    a_entity
    ##make curves
  })

}


## Utility assets
sky <- a_entity(tag = "sky",
                color = "#000")

lighting_model <- a_entity(children = list(
                             a_entity("light",
                                     type = "ambient",
                                     intensity = 0.73),
                            a_entity("light",
                                     type = "point",
                                     position = c(1.5, 16, -5),
                                     intensity = 0.53)
                          ))

controls <- a_pc_control_camera(position = c(-3.2, 22.2, 34))

## Points
## Make coords relative to centre of mesh
sighting_matrix[, 1] <- sighting_matrix[, 1] - mean(acp_trimesh$P[ ,1])
sighting_matrix[, 2] <- sighting_matrix[, 2] - mean(acp_trimesh$P[ ,2])
sighting_matrix[, 3] <- sighting_matrix[, 3] - mean(acp_trimesh$P[ ,3])


## Koala model

sighting_points <- pmap(as.data.frame(sighting_matrix),
                        function(x,y,z){
                          a_entity(tag = "sphere",
                                   position = c(x, y, z),
                                   radius = 10,
                                   color = "#00b6ff")
                        })


koala_path <- a_entity(tag = "curve",
                       id = "track1",
                       js_sources = list("https://rawgit.com/protyze/aframe-curve-component/master/dist/aframe-curve-component.min.js","https://rawgit.com/protyze/aframe-alongpath-component/master/dist/aframe-alongpath-component.min.js"),
                       children = list(
                         a_entity("curve-point", position = c(-2, 2, 0)),
                         a_entity("curve-point", position = c(0, 2, 0)),
                         a_entity("curve-point", position = c(2, 2, 0)),
                         a_entity("curve-point", position = c(0, 2, 0)),
                         a_entity("curve-point", position = c(-2, 2, 0))
                       ))


acp_model <- a_json_model(src_asset = acp_asset,
                          mesh_smooth = TRUE,
                          rotation = c(-90, 0, 0),
                          scale = c(1, 1, 1) * MESH_FOOTPRINT_SCALE,
                          position = c(0,
                                       0 + height_correction * MESH_FOOTPRINT_SCALE,
                                       0),
                          children = sighting_points)

aframe_scene <-
  a_scene(template = "empty",
          title = "Alexander Clark Park",
          description = "An A-Frame scene of ACP",
          children = list(acp_model,
                          sky,
                          controls,
                          lighting_model,
                          koala_path,
                          koala))

aframe_scene$serve(port = 8080)

aframe_scene$stop()

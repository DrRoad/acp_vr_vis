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
MAP_TO_AF_ROT_MATRIX <- matrix(c(1, 0, 0, 0, 0, 1, 0, -1, 0),
                               byrow = FALSE, ncol = 3, nrow = 3)
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

## Image Wrangling

################################################################################

## signtings 2 -> sp56
## sighting 3 -> sp46
## sighting 4 -> sp53
## sighting 1 -> sp50

################################################################################

## Scene construction

################################################################################


## Helper assets
## xy: an xy coordinate
a_roaming_koala <- function(xyz, koala_id, radius = 1.5, n_paths = 3, transit_time = 9000){

  ## offset the height so they're standing
  xyz[2] <- xyz[2] + 1.5

  ## A koala 
  a_koala <- purrr::partial(a_entity,
                            id = koala_id,
                            gltf_model = a_asset("koala", src = "./data/Mesh_Koala.gltf",
                                                 parts = c("./data/Mesh_Koala.bin",
                                                           "./data/Tex_Koala.png")),
                            position = xyz,
                            scale = c(1, 1, 1) * 0.03,
                            rotation = c(0,0,0))

  ## Make curves
  theta <- runif(n_paths)
  x <- cos(theta * 2 * pi) * radius
  z <- sin(theta * 2 * pi) * radius

  ## make the track points
  track_points <- pmap(list(x, z), function(x, z){
    list(
      a_entity("curve-point", position = c(xyz[1], xyz[2], xyz[3])),
      a_entity("curve-point", position = c(xyz[1] + x, xyz[2], xyz[3] + z))
    )
  }) %>% flatten()

  ## make the curve
  curve <- a_entity("curve",
                    id = paste0(koala_id,"track"),
                    children = track_points, 
                    js_sources = list("https://rawgit.com/protyze/aframe-curve-component/master/dist/aframe-curve-component.min.js","https://rawgit.com/protyze/aframe-alongpath-component/master/dist/aframe-alongpath-component.min.js"))


  my_koala <- a_koala(alongpath = list(curve = paste0("#",koala_id,"track"),
                           loop = TRUE,
                           rotate = TRUE,
                           dur = transit_time),
                     children = list(curve))
}


## Utility assets
sky <- a_entity(tag = "sky",
                id = "sky",
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

controls <- a_entity(id = "rig",
                     movement_controls = list(fly = TRUE, easingY = 15,
                                              acceleration = 100),
                     js_sources = list("https://cdn.rawgit.com/donmccurdy/aframe-extras/v4.0.2/dist/aframe-extras.controls.js"),
                     position = c(-3.2, 22.2, 34),
                     children = list(
                       a_entity(camera = "",
                                position = c(0, 1.6, 0),
                                look_controls = list(pointerLockEnabled = TRUE),
                                children = list(
                                  a_entity("cursor", cursor = list(fuse = TRUE))
                                ))
                     ))

## 360 Panorama
pano <- a_asset(tag = "img", "pano", src = "./data/SP53.jpg")

## Points
## Make coords relative to centre of mesh
points_matrix <-
  cbind(sighting_matrix[, 1] - mean(acp_trimesh$P[ ,1]),
        sighting_matrix[, 2] - mean(acp_trimesh$P[ ,2]),
        sighting_matrix[, 3] - mean(acp_trimesh$P[ ,3])) %*%
  MAP_TO_AF_ROT_MATRIX *
  MESH_FOOTPRINT_SCALE +
matrix(c(rep(0,nrow(sighting_matrix)),
       rep(1, nrow(sighting_matrix)),
       rep(0, nrow(sighting_matrix))),
       byrow = FALSE, nrow = nrow(sighting_matrix)
       ) * height_correction * MESH_FOOTPRINT_SCALE


  

colnames(points_matrix) <- c("x", "y", "z")

## Sighting Points
sighting_points <- pmap(as.data.frame(points_matrix),
                        function(x, y, z){
                          a_entity(tag = "sphere",
                                   position = c(x, y, z), ## swap axis since our z is aframe y
                                   radius = 2,
                                   color = "#00b6ff",
                                   material = list(opacity = 0.5),
                                   js_sources = list("https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js"),
                                   event_set__click = list(`_event` = "click",
                                                           `_target` = "#sky",
                                                           src = pano,
                                                           color = "")
                                   )
                        })

## Roaming Koalas
koala_df <- cbind(as.data.frame(points_matrix),
                  list(id = letters[seq(nrow(points_matrix))]),
                  stringsAsFactors = FALSE)

koalas <- pmap(koala_df,
               function(x, y, z, id){
                 a_roaming_koala(xyz = c(x, y, z),
                                 koala_id = id,
                                 radius = runif(1, 35, 55) * MESH_FOOTPRINT_SCALE,
                                 transit_time = rnorm(1, 18000, 3000)
                                 )
               })

points_plot <- a_entity(children = c(sighting_points, koalas))


acp_model <- a_json_model(src_asset = acp_asset,
                          mesh_smooth = TRUE,
                          rotation = c(-90, 0, 0),
                          scale = c(1, 1, 1) * MESH_FOOTPRINT_SCALE,
                          position = c(0,
                                       0 + height_correction * MESH_FOOTPRINT_SCALE,
                                       0))

aframe_scene <-
  a_scene(template = "empty",
          title = "Alexander Clark Park",
          description = "An A-Frame scene of ACP",
          children = list(acp_model,
                          sky,
                          controls,
                          lighting_model,
                          points_plot))

aframe_scene$serve(port = 8081)

aframe_scene$stop()

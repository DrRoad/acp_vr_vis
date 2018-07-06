################################################################################

### Prerequisites

################################################################################

devtools::install_github("mdsumner/guerrilla")

library(sf)
library(raster)
library(rgl)
library(tidyverse)

source("./helpers/contour_to_raster.R")

################################################################################

### Visualisation

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

plot(acp_elev_raster)
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



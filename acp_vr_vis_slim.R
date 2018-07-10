################################################################################

## Prerequisites - built in acp_vr_vis.R

################################################################################


library(tidyverse)
library(r2vr)
read_rds("./data/scene_data_objects.Rds") %>%
  iwalk(~assign(x = .y, value = .x, envir = globalenv()))

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
                     position = c(0.8, 32, 49),
                     children = list(
                       a_entity(camera = "",
                                position = c(0, 1.6, 0),
                                look_controls = list(pointerLockEnabled = TRUE),
                                children = list(
                                  a_entity("cursor", cursor = list(fuse = TRUE))
                                ))
                     ))

## 360 Panoramas

## signtings 2 -> sp56
## sighting 3 -> sp46
## sighting 4 -> sp53
## sighting 1 -> sp50

pano_images <- imap(c("./data/SP50.jpg",
                      "./data/SP56.jpg",
                      "./data/SP46.jpg",
                      "./data/SP53.jpg"),
                    ~a_asset(tag = "img", paste0("pano",.y), src = .x))




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

points_df <- as_tibble(points_matrix) %>%
  add_column(image = pano_images)

## Sighting Points
sighting_points <- pmap(points_df,
                        function(x, y, z, image){
                          a_entity(tag = "sphere",
                                   position = c(x, y, z), ## swap axis since our z is aframe y
                                   radius = 2,
                                   color = "#00b6ff",
                                   material = list(opacity = 0.5),
                                   js_sources = list("https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js"),
                                   event_set__click = list(`_event` = "click",
                                                           `_target` = "#sky",
                                                           src = image,
                                                           color = ""),
                                   event_set__mouseenter = list(`_event` = "mouseenter",
                                                                color = "#fff432"),

                                   event_set__mouseleave = list(`_event` = "mouseleave",
                                                                color = "#00b6ff")
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

acp_asset <- a_in_mem_asset(data = list(acp_tex_json, readr::read_file_raw(texfile)),
                            id = "acp",
                            src = "./data/acp.json",
                            parts = texfile)


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
 
##aframe_scene$stop()

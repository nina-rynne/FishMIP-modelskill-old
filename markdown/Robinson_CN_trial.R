
rm(list = ls())

library(here)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)

###### example with one model only ----

world <- ne_countries(scale = 'small', returnclass = 'sf')

ggplot() +
  geom_sf(data = world)

# trial 
trial <- st_transform(world,crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

ggplot() +
  geom_sf(data = trial) 

# PROBLEM when transformed in Robinson: 
# 1) horizontal lines across the globe
# 2) sf object get centered around the 180 meridian 

# SOLUTION to 1)
# https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2
# NOTE code for your other maps (e.g. EC) is different and does lead to this error for land but 
# you cannot use that code to fix the LMEs data below ... (tryed - not working) 
# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
world2 <- world %>% sf::st_difference(polygon)

# ERROR
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 0 crosses edge 78

# SOLUTION 
# https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 
sf_use_s2(FALSE)
world2 <- world %>% sf::st_difference(polygon)

# WARNING 
# although coordinates are longitude/latitude, st_difference assumes that they are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

# perform transformation on modified version of world dataset
world_robinson <- st_transform(world2, 
                               crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# plot
ggplot() +
  geom_sf(data = world_robinson) 

## your data 
stats_byLME_shaped <- readRDS(here("output/stats_byLME_shaped.rds"))

data <-stats_byLME_shaped %>% 
  filter(Model == "boats ipsl-cm6a-lr") # select one model

# do as above for land
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
data2 <- data %>% sf::st_difference(polygon)

# ERROR
# although coordinates are longitude/latitude, st_difference assumes that they are planar
# Error in (function (msg)  :
#             TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point 10.979439735542712 54.380550384184289 at 10.979439735542712 54.380550384184289

# SOLUTION
# https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
data <- data %>% sf::st_buffer(0)

# WARNING
# dist is assumed to be in decimal degrees (arc_degrees).
# Warning message:
#   In st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle = endCapStyle,  :
#                      st_buffer does not correctly buffer longitude/latitude data

data2 <- data %>% st_difference(polygon) 

# WARNING
# although coordinates are longitude/latitude, st_difference assumes that they are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

# perform transformation on modified version of world dataset
data_robinson <- st_transform(data2, 
                              crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# plot
ggplot() +
  geom_sf(data = data_robinson, aes(fill = Correlation))+
  geom_sf(data = world_robinson)

## try with full dataset and all models ----
## warning and errors are the same as above 

# rm(list = ls())
# 
# library(here)
# library(rnaturalearth)
# library(sf)
# library(ggplot2)
# library(tidyverse)

# download world data and center around 180 meridian as per fixed polygon below 
world <- ne_countries(scale = 'small', returnclass = 'sf')

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
world2 <- world %>% sf::st_difference(polygon)

# https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 
sf_use_s2(FALSE)
world2 <- world %>% sf::st_difference(polygon)

# perform transformation on modified version of world dataset
world_robinson <- st_transform(world2, 
                               crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# plot
ggplot() +
  geom_sf(data = world_robinson)

## your data 
stats_byLME_shaped <- readRDS(here("output/stats_byLME_shaped.rds"))

data <-stats_byLME_shaped 

# fix: https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
data <- data %>% sf::st_buffer(0)
data2 <- data %>% st_difference(polygon) 

# perform transformation on modified version of world dataset
data_robinson <- st_transform(data2, 
                              crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

## plot

MEM_names <- c('boats' = "BOATS", 'ecoocean' = "EcoOcean")
ESM_names <- c('gfdl-esm2m' = "GFDL-ESM2M", 
               'gfdl-esm4' = "GFDL-ESM4", 
               'ipsl-cm5a-lr' = "IPSL-CM5A-LR", 
               'ipsl-cm6a-lr' = "IPSL-CM6A-LR")

p1 <- ggplot() +
  geom_sf(data = data_robinson, aes(fill = Correlation, geometry = geometry), size = NA) +
  geom_sf(data = world_robinson, color = NA, fill = "grey93", size = 0.15)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0, limits = c(-1,1), breaks = c(-1, 0, 1), name = "Correlation\n") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size=10, vjust = -2),
        plot.title.position = "panel",
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=8),
        legend.box.spacing = unit(0.2, "cm")
  )+
  facet_grid(ESMs ~ MEMs,
             labeller = labeller(ESMs = as_labeller(ESM_names), MEMs = as_labeller(MEM_names)))

ggsave(plot = p1, filename = here("output/figures/LMEmap_correlation_CN.jpg"),
       device = "jpg", dpi = 600, width = 15, height = 12, units="cm")

# ##### Add border for ocean ----
# # https://stackoverflow.com/questions/75860459/change-ocean-color-in-robinson-projection-map
# ocean <- st_polygon(
#   list(
#     cbind(
#       c(seq(-180, 179, len = 100), rep(180, 100), seq(179, -180, len = 100), rep(-180, 100)),
#       c(rep(-90, 100), seq(-89, 89, len = 100), rep(90, 100), seq(89, -90, len = 100))))) %>% 
#   st_sfc(crs = "WGS84") %>% 
#   st_as_sf()
# 
# ggplot() +
#   geom_sf(data=data3, aes(fill = Correlation)) +
#   geom_sf(data=wmap, color = NA, fill = "grey93", size = 0.15)+
#   geom_sf(data=ocean, fill = NA) + 
#   coord_sf(crs= robCRS, expand = F) +
#   scale_fill_gradient2(low ="red", 
#                        mid = "white", 
#                        high = "blue", 
#                        midpoint = 0, 
#                        limits = c(-1,1), 
#                        breaks = c(-1, 0, 1), 
#                        name = "Correlation\n")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         plot.title = element_text(hjust = 0.5, size=10, vjust = -2),
#         plot.title.position = "panel",
#         legend.position = "right",
#         legend.direction = "vertical",
#         legend.title = element_text(size = 8),
#         legend.text = element_text(size=8),
#         legend.box.spacing = unit(0.2, "cm")
#   )
# 


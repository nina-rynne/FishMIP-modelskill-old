
rm(list = ls())

library(here)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(tidyverse)

###### example  ----

world <- ne_countries(scale = 'small', returnclass = 'sf')

ggplot() +
  geom_sf(data = world)

# trial 
trial <- st_transform(world,crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

ggplot() +
  geom_sf(data = trial) # when transformed in Robinson, sf object get centered around the 180 meridian 

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

ggplot() +
  geom_sf(data = world2) # line to divide the 0 meridian... 

# not working - the ususal error ... not sure why this is comunig up all the times
# https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 
sf_use_s2(FALSE)
world2 <- world %>% sf::st_difference(polygon)

# perform transformation on modified version of world dataset
world_robinson <- st_transform(world2, 
                               crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# plot
ggplot() +
  geom_sf(data = world_robinson) # now projections around the 180 meridian  

## your data 
stats_byLME_shaped <- readRDS(here("output/stats_byLME_shaped.rds"))

data <-stats_byLME_shaped %>% 
  filter(Model == "boats ipsl-cm6a-lr") #, 
# LME_NUMBER == 1)

polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
# data2 <- data %>% sf::st_difference(polygon)

# not working - the ususal error ... not sure why this is comunig up all the times
# https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 
sf_use_s2(FALSE)
# data2 <- data %>% sf::st_difference(polygon)

# error TopologyException
# fix: https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
data <- data %>% sf::st_buffer(0)
data2 <- data %>% st_difference(polygon) # Warning message: attribute variables are assumed to be spatially constant throughout all geometries
#### WARNING lots pf warnings and assumptions 

# perform transformation on modified version of world dataset
data_robinson <- st_transform(data2, 
                              crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# plot
ggplot() +
  geom_sf(data = data_robinson)

wmap<-ne_download(scale = 110, type = 'countries', returnclass = "sf")
wmap_robinson <- st_transform(wmap, 
                              crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

ggplot() +
  geom_sf(data = data_robinson, fill = "blue")+
  geom_sf(data = wmap_robinson)

### the problem here is that I am centering everything around the 180 deg... 
# solution but nont very happy with this 
# also see how to delete lined splitting polygons: 
# https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 

ggplot() +
  geom_sf(data = data_robinson, aes(fill = Correlation))+
  geom_sf(data = world_robinson)


### center projections around meridian 0 -----
world <- ne_countries(scale = 'small', returnclass = 'sf')

ggplot() +
  geom_sf(data = world)

### what if I use a combination of sf and geom_polygon following EC code?? 

# to plot in lat/long
wmap<-ne_download(scale = 110, type = 'countries')
# wmap_df_latLon <- fortify(wmap)

# to plot in Robinson projections 
library(rgdal) 
wmap_robin <- spTransform(wmap, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
wmap_df_robin <- fortify(wmap_robin)

ggplot() +
  geom_sf(data = data_robinson, aes(fill = Correlation))+
  geom_polygon(data=wmap_df_robin, aes(x=long, y=lat, group=group),
               fill="grey20", color=NA, linewidth = 0.25) 

# still need to turn this around 
# https://gis.stackexchange.com/questions/332106/union-anti-meridian-multipolygons-after-re-centering-world-map


## Robinson projections - CN try with full dataset ----

rm(list = ls())

library(here)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(tidyverse)

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

# not working - the ususal error ... not sure why this is comunig up all the times
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

data <-stats_byLME_shaped # %>% 
# filter(Model == "boats ipsl-cm6a-lr") #, 
# LME_NUMBER == 1)

# error TopologyException
# fix: https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
data <- data %>% sf::st_buffer(0)
data2 <- data %>% st_difference(polygon) # Warning message: attribute variables are assumed to be spatially constant throughout all geometries
#### lots pf warnings and assumptions here ...!!!!

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






## Robinson projections - CN centered around 0 but problem not solved ----


## Load data ----
rm(list = ls())

# load data
stats_byLME_shaped <- readRDS(here("output/stats_byLME_shaped.rds"))

#### tentative 1 ----
# define Robinson projections
# robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
robCRS<-"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# dowload world map
wmap<-ne_download(scale = 110, type = 'countries', returnclass = "sf")
# wmap_robin <- spTransform(wmap, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 
# wmap_df_robin <- fortify(wmap_robin)
# wmap<-st_transform(wmap, crs = st_crs(robCRS))

# convert rasterLayer to sf object and consider Robinson projections
# library(raster)
data<-stats_byLME_shaped
# robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# data <- rasterToPolygons(data) # convert Rasterlayer to spatial polygon dataframe 
# data <- st_as_sf(data) # convert dataframe to sf object 
# data <- st_transform(data, crs = st_crs(robCRS)) # convert sf object to Robinson Projection

class(wmap)
class(data)
st_crs(wmap) 
st_crs(data)

## Plot data trial ----
# try with one esm/mem model combination only 
data <-data %>% 
  filter(Model == "boats ipsl-cm6a-lr")

ggplot() +  
  geom_sf(data=data, colour = NA, aes(fill = Correlation)) +
  geom_sf(data=wmap, color = NA, fill = "grey93", linewidth = 0.15)+
  # coord_sf(crs = robCRS, expand = F) +
  scale_fill_gradient2(low ="red", 
                       mid = "white", 
                       high = "blue", 
                       midpoint = 0, 
                       limits = c(-1,1), 
                       breaks = c(-1, 0, 1), 
                       name = "Correlation\n")

## Correct polygons that cross the meridian line ----
# problem: https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2
# and solution 

# identify problematic polygons
# data %>% filter(LME_NUMBER == 61) # LME == 61 but Object ID == 47 ... to investigate.
# data %>% filter(LME_NUMBER == 42) # LME == 42 but Object ID == 46 ... this was done correctly in old files ... investigate
data2<-data %>% filter(LME_NUMBER %in% c(1,54,65)) # 1. east bering sea; 54. Chukchi Sea; 65. Aleutian island - all crossing the line. But 64 and 61 Arctic and Antarctic) seem OK.

ggplot() +
  geom_sf(data=data2, colour = NA, aes(fill = Correlation)) +
  geom_sf(data=wmap, color = NA, fill = "grey93", linewidth = 0.15)+
  # coord_sf(crs = robCRS, expand = F) +
  scale_fill_gradient2(low ="red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,
                       limits = c(-1,1),
                       breaks = c(-1, 0, 1),
                       name = "Correlation\n")

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of your data
polygon <- st_polygon(
  list(
    rbind(
      c(-0.0001, 90),
      c(0, 90),
      c(0, -90),
      c(-0.0001, -90),
      c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(st_crs(data))

# data <- st_transform(data, crs = st_crs(robCRS)) # convert sf object to Robinson Projection

st_crs(data)
st_crs(polygon)

# modify world dataset to remove overlapping portions with world's polygons
data <- data %>% st_difference(polygon)

# error 
# fix: https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
data <- data %>% sf::st_buffer(0)
data3 <- data %>% st_difference(polygon) # Warning message: attribute variables are assumed to be spatially constant throughout all geometries - CN to warry about?? 

# plot 
ggplot() +  
  geom_sf(data=data3, colour = NA, aes(fill = Correlation)) +
  geom_sf(data=wmap, color = NA, fill = "grey93", linewidth = 0.15)+
  coord_sf(crs = robCRS, expand = F) +
  scale_fill_gradient2(low ="red", 
                       mid = "white", 
                       high = "blue", 
                       midpoint = 0, 
                       limits = c(-1,1), 
                       breaks = c(-1, 0, 1), 
                       name = "Correlation\n")


##### Add border for ocean ----
# https://stackoverflow.com/questions/75860459/change-ocean-color-in-robinson-projection-map
ocean <- st_polygon(
  list(
    cbind(
      c(seq(-180, 179, len = 100), rep(180, 100), seq(179, -180, len = 100), rep(-180, 100)),
      c(rep(-90, 100), seq(-89, 89, len = 100), rep(90, 100), seq(89, -90, len = 100))))) %>% 
  st_sfc(crs = "WGS84") %>% 
  st_as_sf()

ggplot() +
  geom_sf(data=data3, aes(fill = Correlation)) +
  geom_sf(data=wmap, color = NA, fill = "grey93", size = 0.15)+
  geom_sf(data=ocean, fill = NA) + 
  coord_sf(crs= robCRS, expand = F) +
  scale_fill_gradient2(low ="red", 
                       mid = "white", 
                       high = "blue", 
                       midpoint = 0, 
                       limits = c(-1,1), 
                       breaks = c(-1, 0, 1), 
                       name = "Correlation\n")+
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
  )



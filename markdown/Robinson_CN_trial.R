
rm(list = ls())

library(here)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(colorspace)
library(parallel)
library(patchwork)

## load land and define projections ----

world <- ne_countries(scale = 'small', returnclass = 'sf')
robCRS<-"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# can center around antimeridian too but lots of issues... 
# see for some solutions: # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2
# robCRS<-'+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# ggplot() +
#   geom_sf(data = world)+
#   coord_sf(crs= robCRS, expand = F) 

# ## example with one model only ----
# stats_byLME_shaped <- readRDS(here("output/stats_byLME_shaped.rds"))
# 
# data <-stats_byLME_shaped %>%
#   filter(Model == "boats ipsl-cm6a-lr") # select one model
# 
# # ggplot() +
# #   geom_sf(data = world)+
# #   geom_sf(data = data, aes(fill = Correlation))+
# #   coord_sf(crs= robCRS, expand = F) 
# 
# # PROBLEM when transformed in Robinson: horizontal lines across the globe
# 
# # # identify problematic polygons
# # # LME 1, 54, 64, 65
# # # NOTE these are polygons that cross the antimerian (180) 
# # # not the meridian (0) for which there are other solutions usefull when plotting around the 180 meridia (pacific-centered)
# # 
# # ggplot() +
# #   geom_sf(data = data, aes(fill = Correlation))+
# #   coord_sf(crs= robCRS, expand = F) +
# #   facet_wrap(~LME_NUMBER)
# # 
# # ggplot()+
# #   geom_sf(data = data, aes(fill = LME_NUMBER))+
# #   facet_wrap(~LME_NUMBER)
# 
# # # SOLUTION 
# # # https://stackoverflow.com/questions/55162548/is-there-a-better-way-for-handling-spatialpolygons-that-cross-the-antimeridian
# # 
# # data2<-data %>%
# #   st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
# #   st_union()
# #
# # ERROR
# # Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# # Loop 0 is not valid: Edge 11604 has duplicate vertex with edge 11610
# #
# # SOLUTION 
# # https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 
# sf_use_s2(FALSE)
# # 
# # # ERROR 
# # # although coordinates are longitude/latitude, st_union assumes that they are planar
# # # Warning messages:
# # #   1: In CPL_wrap_dateline(st_geometry(x), options, quiet) :
# # #   GDAL Error 1: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -161.34730529788163 58.66580963120532 at -161.34730529788163 58.66580963120532
# # # 2: In CPL_wrap_dateline(st_geometry(x), options, quiet) :
# # #   GDAL Error 1: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -161.34730529788163 58.66580963120532 at -161.34730529788163 58.66580963120532
# # 
# # # SOLUTION
# # # https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
# # data2<-data %>%
# #   sf::st_buffer(0) %>% 
# #   st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
# #   st_union()
# # 
# # # WARNING 
# # # dist is assumed to be in decimal degrees (arc_degrees).
# # # although coordinates are longitude/latitude, st_union assumes that they are planar
# # # Warning message:
# # #   In st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle = endCapStyle,  :
# # #                      st_buffer does not correctly buffer longitude/latitude data
# # 
# # # ALSO all attributes are missing
# # # SOLUTION: remove st_union() 
# 
# data2<-data %>%
#   sf::st_buffer(0) %>% 
#   st_wrap_dateline(options = c("WRAPDATELINE=YES"))
# 
# # still WARNING 
# # dist is assumed to be in decimal degrees (arc_degrees).
# # Warning message:
# #   In st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle = endCapStyle,  :
# #                      st_buffer does not correctly buffer longitude/latitude data
# 
# # ggplot() +
# #   geom_sf(data = data2, aes(fill = Correlation))+
# #   coord_sf(crs= robCRS, expand = F) 
# 
# # make better plot 
# 
# # add Ocean 
# # https://stackoverflow.com/questions/75860459/change-ocean-color-in-robinson-projection-map
# ocean <- st_polygon(
#   list(
#     cbind(
#       c(seq(-180, 179, len = 100), rep(180, 100), seq(179, -180, len = 100), rep(-180, 100)),
#       c(rep(-90, 100), seq(-89, 89, len = 100), rep(90, 100), seq(89, -90, len = 100))))) %>%
#   st_sfc(crs = "WGS84") %>%
#   st_as_sf()
# 
# ggplot()+
#   geom_sf(data = world, color = NA, fill = "grey90", size = 0.15)+
#   geom_sf(data = data2, aes(fill = Correlation))+ 
#   # scale_fill_gradient2(low ="red",
#   #                      mid = "white", 
#   #                      high = "blue", 
#   #                      midpoint = 0, 
#   #                      limits = c(-1,1), 
#   #                      breaks = c(-1, 0, 1), 
#   #                      name = "Correlation\n") +
#   # better colorscale 
#   # HCL-Based Continuous Flexible Diverging Scales for ggplot2
#   scale_fill_continuous_divergingx(palette = 'Geyser', # ArmyRose; Geyser
#                                    mid = 0,
#                                    guide = guide_colorbar(
#                                      title = "Correlation",
#                                      title.position = "top",
#                                      title.hjust = 0.5,
#                                      rev = TRUE), # if palette = 'Geyser', this puts green on top and red on bottom 
#                                    na.value = "grey50") +
#   theme_bw()+
#   theme(
#     panel.grid.major = element_line(color = 'gray75', # instead of ocean and/or to have main loat/lon lines
#                                     linetype = "dashed",
#                                     linewidth = 0.25),
#     panel.background = element_rect(fill = 'white'),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     plot.title = element_text(hjust = 0.5, size=10, vjust = -2),
#     plot.title.position = "panel",
#     legend.position = "right",
#     legend.direction = "vertical",
#     legend.title = element_text(size = 8),
#     legend.text = element_text(size=8),
#     legend.box.spacing = unit(0.2, "cm"))+
#   geom_sf(data = ocean, fill = NA, color = "black")+ # "#8080ff80"
#   coord_sf(crs= robCRS, expand = F) 
# 

## function to plot all models ----

# NOTE errors and warnings after error fixes are the same as aboveexample with one model only
stats_byLME_shaped <- readRDS(here("output/stats_byLME_shaped.rds"))

# add Ocean polygon  
# https://stackoverflow.com/questions/75860459/change-ocean-color-in-robinson-projection-map
ocean <- st_polygon(
  list(
    cbind(
      c(seq(-180, 179, len = 100), rep(180, 100), seq(179, -180, len = 100), rep(-180, 100)),
      c(rep(-90, 100), seq(-89, 89, len = 100), rep(90, 100), seq(89, -90, len = 100))))) %>%
  st_sfc(crs = "WGS84") %>%
  st_as_sf()

plot_correlation<-function(data, 
                           ocean = ocean, 
                           robCRS = robCRS){
  
  # # trial 
  # data = models[[1]]
  
  # # PROBLEM when transformed in Robinson: horizontal lines across the globe
  # # SOLUTION 
  # # https://stackoverflow.com/questions/55162548/is-there-a-better-way-for-handling-spatialpolygons-that-cross-the-antimeridian
  # 
  # data2<-data %>%
  #   st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
  #   st_union()
  #
  # ERROR
  # Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
  # Loop 0 is not valid: Edge 11604 has duplicate vertex with edge 11610
  #
  # SOLUTION 
  # https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using also see SO code 
  sf_use_s2(FALSE)
  # 
  # # ERROR 
  # # although coordinates are longitude/latitude, st_union assumes that they are planar
  # # Warning messages:
  # #   1: In CPL_wrap_dateline(st_geometry(x), options, quiet) :
  # #   GDAL Error 1: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -161.34730529788163 58.66580963120532 at -161.34730529788163 58.66580963120532
  # # 2: In CPL_wrap_dateline(st_geometry(x), options, quiet) :
  # #   GDAL Error 1: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -161.34730529788163 58.66580963120532 at -161.34730529788163 58.66580963120532
  # 
  # # SOLUTION
  # # https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
  # data2<-data %>%
  #   sf::st_buffer(0) %>% 
  #   st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
  #   st_union()
  # 
  # # WARNING 
  # # dist is assumed to be in decimal degrees (arc_degrees).
  # # although coordinates are longitude/latitude, st_union assumes that they are planar
  # # Warning message:
  # #   In st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle = endCapStyle,  :
  # #                      st_buffer does not correctly buffer longitude/latitude data
  # 
  # # ALSO all attributes are missing
  # # SOLUTION: remove st_union() 
  data2<-data %>%
    sf::st_buffer(0) %>% 
    st_wrap_dateline(options = c("WRAPDATELINE=YES"))
  #
  # still WARNING 
  # dist is assumed to be in decimal degrees (arc_degrees).
  # Warning message:
  #   In st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle = endCapStyle,  :
  #                      st_buffer does not correctly buffer longitude/latitude data
  #
  # ggplot() +
  #   geom_sf(data = data2, aes(fill = Correlation))+
  #   coord_sf(crs= robCRS, expand = F) 
  
  # make better plot 
  gg_map_corr<-ggplot()+
    geom_sf(data = world, color = NA, fill = "grey90", size = 0.15)+
    geom_sf(data = data2, aes(fill = Correlation))+ 
    # scale_fill_gradient2(low ="red",
    #                      mid = "white", 
    #                      high = "blue", 
    #                      midpoint = 0, 
    #                      limits = c(-1,1), 
    #                      breaks = c(-1, 0, 1), 
    #                      name = "Correlation\n") +
    # better colorscale 
    # HCL-Based Continuous Flexible Diverging Scales for ggplot2
    scale_fill_continuous_divergingx(palette = 'Geyser', # ArmyRose; Geyser
                                     mid = 0,
                                     guide = guide_colorbar(
                                       title = "Correlation",
                                       title.position = "top",
                                       title.hjust = 0.5,
                                       rev = TRUE), # if palette = 'Geyser', this puts green on top and red on bottom 
                                     na.value = "grey50") +
    theme_bw()+
    theme(
      panel.grid.major = element_line(color = 'gray75', # instead of ocean and/or to have main loat/lon lines
                                      linetype = "dashed",
                                      linewidth = 0.25),
      panel.background = element_rect(fill = 'white'),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      # plot.title = element_text(hjust = 0.5, size=10, vjust = -2),
      # plot.title.position = "panel",
      text = element_text(size = 7), 
      legend.title=element_text(size = 8), 
      legend.text=element_text(size = 7),
      # legend.position = "right",
      # legend.direction = "vertical",
      # legend.box.spacing = unit(0.2, "cm"), 
      legend.key.height = unit(0.2, "cm"), # this becomes width if you choose bottom position
      legend.key.width = unit(1, "cm"),
      legend.position= 'bottom')+
    geom_sf(data = ocean, fill = NA, color = "black")+ # "#8080ff80"
    coord_sf(crs= robCRS, expand = F) 

  ## add ocean and adjust as above 
  return(gg_map_corr = gg_map_corr)
}

## run function above in // ----
models<-split(stats_byLME_shaped,stats_byLME_shaped$Model)
# names(models)

# # try function for one model only 
# p1<-plot_correlation(data = models[[1]], 
#                      ocean = ocean, 
#                      robCRS = robCRS)

# do in // 
p1<-mclapply(models, function(x) plot_correlation(x, ocean = ocean, robCRS = robCRS), mc.cores = detectCores()-2)
names(p1)<-names(models)

# check plots 
p1$`boats gfdl-esm2m`

### print ----

# set plot order and add panel notations - better way of doing this but no time
names(models)
new_names<-(c("a) BOATS GFDL-esm2m",
            "c) BOATS GFDL-esm4",
            "e) BOATS IPSL-cm5a-lr",
            "g) BOATS IPSL-cm6a-lr",
            "b) EcoOcean GFDL-esm2m",
            "d) EcoOcean GFDL-esm4",
            "f) EcoOcean IPSL-cm5a-lr",
            "h) EcoOcean IPSL-cm6a-lr"))

names(p1)<-new_names

for(i in 1:length(p1)){
  p1[[i]]<-p1[[i]]+
    ggtitle(names(p1)[[i]])+
    theme(plot.title = element_text(hjust = 0, vjust = 2, size = 9))
}

p1$`a) BOATS GFDL-esm2m`

final<-p1$`a) BOATS GFDL-esm2m` +
  p1$`b) EcoOcean GFDL-esm2m`+
  p1$`c) BOATS GFDL-esm4`+
  p1$`d) EcoOcean GFDL-esm4`+
  p1$`e) BOATS IPSL-cm5a-lr`+
  p1$`f) EcoOcean IPSL-cm5a-lr`+
  p1$`g) BOATS IPSL-cm6a-lr`+
  p1$`h) EcoOcean IPSL-cm6a-lr`+
  plot_layout(ncol = 2,
              guides = 'collect')

name = paste("/home/ubuntu/FishMIP-modelskill/output/figures/", Sys.Date(),"_" ,"Fig1_CN.pdf", sep = "")

pdf(name, width = 7, height = 8, bg = "transparent")  
final
dev.off()




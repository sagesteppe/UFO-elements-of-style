library(sf)
library(tidyverse)
library(ggspatial)

p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

acec <- st_read(
  file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), quiet = T)

allotments <- st_read(
  file.path(p2carto, vector_data[grep('*Grazing*', vector_data)]), quiet = T)

administrative_boundaries <- st_read(
  file.path(p2carto, vector_data[grep('*admu*', vector_data)]), quiet = T)

grouse <- st_read(
  file.path(p2carto, vector_data[grep('*Grouse*', vector_data)]), quiet = T)

gtlf_roads <- st_read(
  file.path(p2carto, vector_data[grep('*GTLF*', vector_data)]), quiet = T)

padus <- st_read(
  file.path(p2carto, vector_data[grep('PAD.*Combined*', vector_data)]), quiet = T)

nm_and_nca <- st_read(
  file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), quiet = T)

tabeguache <- st_read(
  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)

wa <- st_read(
  file.path(p2carto, vector_data[grep('*unofficial*', vector_data)]), quiet = T)

wsa <- st_read(
  file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), quiet = T)


extent <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE') %>%
  st_bbox()



public_lands_pal <- setNames(

  c( # colours
    rgb(254, 230, 121, max = 255), # BLM
    rgb(204, 235, 197, max = 255), # USFS
    rgb(202, 189, 220, max = 255), # NPS
    rgb(127, 204, 167, max = 255), # FWS
    rgb(255, 255, 179, max = 255), # USBR
    rgb(253, 180, 108, max = 255), # TRIB
    rgb(251, 180, 206, max = 255), # DOD
    rgb(228, 196, 159, max = 255), # OTHF
    rgb(179, 227, 238, max = 255), # SLB
    rgb(255, 255, 255, max = 255), # PVT
    rgb(143, 181, 190, max = 255) # CITY CNTY
  ), 
  c( # names
    'BLM', 'USFS', 'NPS', 'FWS', 'USBR', 'TRIB', 'DOD', 'OTHF', 'SLB', 'PVT', 'CITY_CNTY_SDC_SDNR_SPR')
)


# Create Basemaps and Templates for the Gunnison Gorge NCA

gg <- filter(nm_and_nca, NLCS_NAME
                    =='Gunnison Gorge National Conservation Area') 
gg_extent <- gg %>% 
  st_buffer(3218) %>% 
  st_bbox(gg) %>% 
  st_as_sfc() %>% 
  st_as_sf()

ggPad <- st_intersection(gg_extent, padus) %>% 
  filter(!Own_Type %in% c('LOC', 'JNT'))
gg_extent <- st_bbox(gg_extent)

ggPad %>% 
  st_drop_geometry() %>% 
  distinct(Own_Name)
gg_plp <- public_lands_pal[c(unique(ggPad$Own_Name))]
gg_plp <- gg_plp[order(names(gg_plp))]

acec <- st_crop(acec, gg_extent)
gr <- st_make_grid(acec, square = F , flat_topped =  T, n = c(10, 10))
gr <- st_intersection(acec, gr)

ggplot() +
  geom_sf(data = ggPad, aes(fill = Own_Name)) +
  geom_sf(data = gg, color = 'darkgreen', lwd = 1.5, 
          fill = public_lands_pal['BLM']) +
  geom_sf(data = gr, fill = NA, color = 'grey25') +
  geom_sf(data = wa) +
  
  coord_sf(xlim = c(gg_extent['xmin'], gg_extent['xmax']), 
           ylim = c(gg_extent['ymin'], gg_extent['ymax'])) +
  
  scale_fill_manual(values = gg_plp) +
  
  theme_void() +
  
  labs(fill = 'Management:') +
  theme(legend.position = 'bottom') +
  annotation_scale(location = "bl", width_hint = 0.225) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(-0.1, "in"), 
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal)


library(rcartocolor)

  
  
  
  
  
  
  
  
  

#######################

ggplot() +
  geom_sf(data = allotments, fill = NA) +
  geom_sf(data = administrative_boundaries, fill = NA) +
  
  coord_sf(xlim = c(extent['xmin'], extent['xmax']), 
           ylim = c(extent['ymin'], extent['ymax'])) +
  
  theme_void() +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal)


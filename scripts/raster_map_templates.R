library(tmap)
library(sf)
library(tidyverse)
library(terra)
library(ggspatial)


p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

aim_outcomes <- st_read(
  file.path(p2carto, vector_data[grep('*outcomes*', vector_data)]), quiet = T) %>% 
  st_transform(26913)

aim <- st_read(
  file.path(p2carto, vector_data[grep('*Plots*', vector_data)]), quiet = T)

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

streams <- st_read(
  file.path(p2carto, vector_data[grep('*Streams*', vector_data)]), quiet = T)

tabeguache <- st_read(
  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)

wa <- st_read(
  file.path(p2carto, vector_data[grep('*unofficial*', vector_data)]), quiet = T)

wsa <- st_read(
  file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), quiet = T)

extent <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE') %>%
  st_bbox()

design_rast <- '/media/sagesteppe/ExternalHD/UFO_AIM_Design_Stratification/processed'
strat_raster <- terra::rast(
  file.path(design_rast, 'UFO_Strata_reclass_30m.tif'))
lookup_table <- read.csv(file.path(
  design_rast, 'UFO_strata_areas.csv')) %>% 
  select(RasterValue, Code)

strata_pal <- setNames(
  c('#4A5A28', '#ADB1B9', '#CEB88E', '#574039', '#B64841',
    '#1357a6', '#1B1212', '#F9E076', '#39993A', '#00688B'),
  c('PJ', 'SS', 'SD', 'MMS', 'AS', 'RI', 'OT', 'GR', 'PP', 'MC')
)

public_lands_pal <- setNames(
  
  # these manually transcribed from the H-1553-Publications Standards Manual
  # Handbook - hopefully no errors.
  # [H-1553](https://www.ntc.blm.gov/krc/uploads/223/Ownership_Map_Color_Reference_Sheet.pdf)
  
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

strat_raster <- aggregate(strat_raster, fact=5)
str_vals <- strat_raster %>% 
  as.data.frame(xy = TRUE) %>%
  mutate(BPS_CODE = round(BPS_CODE)) %>% 
  left_join(., lookup_table, by = c('BPS_CODE' = 'RasterValue'))

ggplot() +
  geom_tile(data = str_vals, aes(x = x, y = y, fill = Code)) +
  scale_fill_manual(values = strata_pal) +
  labs(fill = 'Stratum', 
       title = '') +
  theme_void()

rm(design_rast)


# GUNNISON GORGE HERE

gg <- filter(nm_and_nca, NLCS_NAME
             =='Gunnison Gorge National Conservation Area') 
extent <- gg %>% 
  st_buffer(3218) %>% 
  st_bbox(gg) %>% 
  st_as_sfc() %>% 
  st_as_sf()

Pad <- st_intersection(extent, padus) %>% 
  filter(!Own_Type %in% c('LOC', 'JNT'))
bbox <- st_bbox(extent)

plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]

ACEC <- st_crop(acec, bbox)
acec_grid <- st_make_grid(ACEC, square = F , flat_topped =  T, n = c(5, 5))
acec_grid <- st_intersection(ACEC, acec_grid)

AIM <- aim %>% mutate(Dumm = 'A') %>% st_crop(., bbox)

ggplot() +
  geom_tile(data = str_vals, aes(x = x, y = y, fill = Code)) +
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.8) +
  geom_sf(data = wa, fill = rgb(254, 204, 92, max = 255), color = NA) +
  geom_sf(data = acec_grid, fill = NA, aes(color = 'grey25'), alpha = 0.8) +
  geom_sf(data = gg, aes(color = 'darkgreen'), lwd = 1.0, 
          fill = NA, alpha = 0.8) +
  geom_sf(data = AIM, aes(shape = Dumm)) +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  scale_fill_manual(values = plp, limits = c("BLM", "NPS", "FWS")) +
  
  theme_void() +
  theme(legend.title.align=0.5,
        title=element_text(hjust =0.5)) +
  
  labs(fill = 'Management', 
       title = 'AIM Plots Sampled in the Vicinity of\nGunnison Gorge NCA') +
  scale_color_identity(name = "Borders",
                       breaks = c("darkgreen", "grey25"),
                       labels = c("NCA", "ACEC"),
                       guide = "legend") +
  scale_shape_manual(name = "",
                     labels = 'AIM',
                     values =  15) +
  annotation_scale(location = "bl", width_hint = 0.225) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(-0.1, "in"), 
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  guides(
    colour = guide_legend(order = 2),
    fill = guide_legend(order = 1),
    shape = guide_legend(order = 3)
  )

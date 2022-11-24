library(tmap)
library(sf)
library(tidyverse)
library(terra)


p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

aim_outcomes <- st_read(
  file.path(p2carto, vector_data[grep('*outcomes*', vector_data)]), quiet = T) %>% 
  st_transform(26913)

acec <- st_read(
  file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), quiet = T)

administrative_boundaries <- st_read(
  file.path(p2carto, vector_data[grep('*admu*', vector_data)]), quiet = T)

padus <- st_read(
  file.path(p2carto, vector_data[grep('PAD.*Combined*', vector_data)]), quiet = T)

nm_and_nca <- st_read(
  file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), quiet = T)

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

strat_raster <- aggregate(strat_raster, fact=5)
str_vals <- strat_raster %>% 
  as.data.frame(xy = TRUE) %>%
  mutate(BPS_CODE = round(BPS_CODE)) %>% 
  left_join(., lookup_table, by = c('BPS_CODE' = 'RasterValue'))

ggplot(str_vals) +
  geom_tile(aes(x = x, y = y, fill = Code)) +
  scale_fill_manual(values = strata_pal) +
  theme_void()

rm(design_rast)


strata_pal <- setNames(
  
  c('#4A5A28', '#ADB1B9', '#CEB88E', '#574039', '#B64841',
    '#1357a6', '#1B1212', '#F9E076', '#39993A', '#00688B'),
  c('PJ', 'SS', 'SD', 'MMS', 'AS', 'RI', 'OT', 'GR', 'PP', 'MC')
  
)

# GUNNISON GORGE HERE

gg <- filter(nm_and_nca, NLCS_NAME == 'Gunnison Gorge National Conservation Area') 
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

AIM <- aim_outcomes %>% mutate(Dumm = 'A') %>% st_crop(., bbox)

st_crs(aim_outcomes)

tm_shape(strat_raster) +
  tm_raster("stratum", palette = terrain.colors(10))

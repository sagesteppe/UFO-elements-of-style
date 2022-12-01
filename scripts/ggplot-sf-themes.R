library(sf)
library(tidyverse)
library(ggspatial)
library(terra)
library(ggnewscale)

p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

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
  file.path(p2carto, vector_data[grep('*NHD_Streams*', vector_data)]), quiet = T)

rivers <- st_read(
  file.path(p2carto, vector_data[grep('*NHD_Rivers*', vector_data)]), quiet = T) %>% 
  st_zm()

tabeguache <- st_read(
  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)

wa <- st_read(
  file.path(p2carto, vector_data[grep('*unofficial*', vector_data)]), quiet = T)

wsa <- st_read(
  file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), quiet = T)

extent <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE') %>%
  st_bbox()

# raster data

raster_data <- list.files(p2carto, recursive = T, pattern = 'tif$')

hill <- rast(
  file.path(p2carto, raster_data[grep('Hill.*fine', raster_data)])
)

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
acec_streams <- st_crop(streams, bbox)
acec_rivers <- st_crop(rivers, bbox)

ggplot(ACEC) +
  geom_sf()

hill <- crop(hill, ext(terra::vect(extent)))
hillshade <- as.data.frame(hill, xy = T)

plp1 <- '#FECC5C'
names(plp1) <- 'Wilderness'
plp1 <- c(plp, plp1)

ggNCA <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(aspect.ratio=1, plot.title = element_text(hjust = 0.5)) +
  ggnewscale::new_scale_fill() +

  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.5) +
  geom_sf(data = wa, fill = '#FECC5C', color = NA, alpha = 0.4) +
  geom_sf(data = acec_grid, fill = NA, aes(color = 'maroon4'), alpha = 0.5) +
  geom_sf(data = gg, aes(color = 'darkgreen'), lwd = 1.0, 
          fill = NA, alpha = 0.5) +
  geom_sf(data = AIM, aes(shape = Dumm), size = 3) +
  geom_sf(data = acec_rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = acec_streams, alpha = 0.2, color = 'blue', lty = 'twodash') +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  theme_void() +
  theme(legend.title.align=0.5,
        title=element_text(hjust = 0.5)) +
  
  labs(fill = 'Management', 
       title = 'AIM Plots sampled near Gunnison Gorge NCA') +
  scale_fill_manual(values = c(plp1),
                    limits = c("BLM", "NPS", "FWS", "Wilderness")) +
  scale_color_identity(name = NULL,  breaks = c("darkgreen", "maroon4"),
                       labels = c("NCA", "ACEC"),  guide = "legend") +
  scale_shape_manual(name = NULL, labels = 'AIM Plots',  values =  18) +
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    shape = guide_legend(order = 3)) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.25, "in"), pad_y = unit(0.3, "in"),
                   width_hint = 0.225) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) 

#setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style')
#ggsave(ggNCA, path = 'results/maps', device = 'png',
#       bg = 'transparent', filename = 'GunnisonGorgeAIMPlots',
#       dpi = 300, width = 4, height = 6 , units = "in")


 # Create Basemaps and Templates for the Dominguez Escalente NCA

####################backed up !!!!!!!!!!


de <- filter(nm_and_nca, NLCS_NAME
             =='Dominguez/Escalante National Conservation Area') 
extent <- de %>% 
  st_buffer(3218) %>% 
  st_bbox(gg) %>% 
  st_as_sfc() %>% 
  st_as_sf()

Pad <- st_intersection(extent, padus) %>% 
  mutate(Own_Name = if_else(Own_Name == 'CITY_CNTY_SDC_SDNR_SPR', 'Local-State', Own_Name))
bbox <- st_bbox(extent)

public_lands_pal1 <- public_lands_pal
names(public_lands_pal1)[11] <- 'Local-State'
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
names(plp)[2] <- 'Local-State'
plp <- plp[1:3]
plp1 <- '#FECC5C'
names(plp1) <- 'Wilderness'
plp1 <- c(plp, plp1)

ACEC <- st_crop(acec, bbox)
acec_grid <- st_make_grid(ACEC, square = F , flat_topped =  T, n = c(25, 25))
acec_grid <- st_intersection(acec, acec_grid)
acec_grid <- st_crop(acec_grid, bbox)
ADMU <- st_crop(administrative_boundaries, bbox)
hill <- crop(hill, ext(terra::vect(extent))) # Reload the native hill dataset
hillshade <- as.data.frame(hill, xy = T)
acec_streams <- st_crop(streams, bbox)
acec_rivers <- st_crop(rivers, bbox)

AIM <- aim %>% mutate(Dumm = 'A') %>% st_crop(., bbox)

ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(aspect.ratio=1, plot.title = element_text(hjust = 0.5)) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.5) +
  geom_sf(data = wa, fill = '#FECC5C', color = NA, alpha = 0.4) +
  geom_sf(data = acec_grid, fill = NA, aes(color = 'maroon4'), alpha = 0.5) +
  geom_sf(data = de, aes(color = 'darkgreen'), lwd = 1.0, 
          fill = NA, alpha = 0.5) +
  geom_sf(data = ADMU, fill = NA, aes(color = 'black'), lwd = 1.0) +
  geom_sf(data = acec_rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = acec_streams, alpha = 0.1, color = 'blue', lty = 'twodash') +
  geom_sf(data = AIM, aes(shape = Dumm), size = 3) +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  theme_void() +
  theme(legend.title.align = 0.5, 
        title=element_text(hjust = 0.5)) +
  
  labs(title = 'AIM Plots sampled near Dominguez-Escalente NCA') +
  scale_fill_manual('Management', values = plp1) +
  scale_color_identity(name = NULL,  breaks = c("darkgreen", "maroon4", "black"),
                       labels = c("NCA", "ACEC", "UFO/GJFO"),  guide = "legend") +
  scale_shape_manual(name = NULL, labels = 'AIM Plots', values =  18) +
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    shape = guide_legend(order = 3)) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.25, "in"), pad_y = unit(0.3, "in"),
                   width_hint = 0.225) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) 

setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style')
ggsave(deNCA, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'DominguezEscalenteAIMPlots',
       dpi = 300, width = 4, height = 6 , units = "in")



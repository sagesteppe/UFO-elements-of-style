library(sf)
library(tidyverse)
library(ggspatial)
library(terra)
library(ggnewscale)
library(cowplot)

p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

aim <- st_read(
  file.path(p2carto, vector_data[grep('*Plots*', vector_data)]), quiet = T)

acec <- st_read(
  file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), quiet = T)

# allotments <- st_read(
#   file.path(p2carto, vector_data[grep('*Grazing*', vector_data)]), quiet = T)

administrative_boundaries <- st_read(
  file.path(p2carto, vector_data[grep('*admu_', vector_data)]), quiet = T)

# grouse <- st_read(
#  file.path(p2carto, vector_data[grep('*Grouse*', vector_data)]), quiet = T)

# gtlf_roads <- st_read(
#  file.path(p2carto, vector_data[grep('*GTLF*', vector_data)]), quiet = T)

padus <- st_read(
  file.path(p2carto, vector_data[grep('PAD.*Fee*', vector_data)]), quiet = T)

mask <- st_read(
  file.path(p2carto, vector_data[grep('*mask*', vector_data)]), quiet = T)

nm_and_nca <- st_read(
  file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), quiet = T)

streams <- st_read(
  file.path(p2carto, vector_data[grep('*NHD_Streams*', vector_data)]), quiet = T)

rivers <- st_read(
  file.path(p2carto, vector_data[grep('*NHD_Rivers*', vector_data)]), quiet = T) %>% 
  st_zm()

#tabeguache <- st_read(
#  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)

wa <- st_read(
  file.path(p2carto, vector_data[grep('*unofficial*', vector_data)]), quiet = T)

wsa <- st_read(
  file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), quiet = T)

bbox <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE') %>%
  st_bbox()

ext_ter <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE') %>%
  terra::vect() |> ext()

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
    'BLM', 'USFS', 'NPS', 'FWS', 'USBR', 'TRIB', 'DOD', 'OTHF', 'SLB', 'Private', 'Local-State')
)

################################################################################
########          AIM Plots sampled near  Gunnison Gorge NCA           #########
################################################################################

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

AIM_gg <- aim %>% mutate(Dumm = 'A') %>% st_crop(., bbox)
acec_streams <- st_crop(streams, bbox)
acec_rivers <- st_crop(rivers, bbox)

hill_gg <- crop(hill, ext(terra::vect(extent)))
hillshade <- as.data.frame(hill_gg, xy = T)

mask_gg <- st_crop(mask, bbox)

plp1 <- '#FECC5C'
names(plp1) <- 'Wilderness'
plp1 <- c(plp, plp1)

ggNCA <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title.align=0.5) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.5) +
  geom_sf(data = wa, fill = '#FECC5C', color = NA, alpha = 0.4) +
  geom_sf(data = acec_grid, fill = NA, aes(color = 'maroon4'), alpha = 0.5) +
  geom_sf(data = gg, aes(color = 'darkgreen'), lwd = 1.0, 
          fill = NA, alpha = 0.5) +
  geom_sf(data = AIM_gg, aes(shape = Dumm), size = 3) +
  geom_sf(data = acec_rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = acec_streams, alpha = 0.2, color = 'blue', lty = 'twodash') +
  geom_sf(data = mask_gg, color = 'white', alpha = 0.7, lwd = 0) +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +

  
  labs(fill = 'Management', 
       title = 'Plots near Gunnison Gorge NCA') +
  scale_fill_manual(values = c(plp1)) +
  scale_color_identity(name = NULL,  breaks = c("darkgreen", "maroon4"),
                       labels = c("NCA", "ACEC"),  guide = "legend") +
  scale_shape_manual(name = NULL, labels = 'AIM Plots',  values =  18) +
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    shape = guide_legend(order = 3)) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.35, "in"), pad_y = unit(0.34, "in"),
                   width_hint = 0.225) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.55, "in"),
                         style = north_arrow_minimal) 

setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style')
ggsave(ggNCA, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'GunnisonGorgeAIMPlots.png',
       dpi = 300,  units = "in")

rm(ggNCA, mask_gg, plp1, plp, gg, hill_gg, extent, bbox, Pad, public_lands_pal1)

################################################################################
########         AIM plots sampled near Dominguez Escalente NCA        #########
################################################################################
de <- filter(nm_and_nca, NLCS_NAME =='Dominguez/Escalante National Conservation Area') 
extent <- de %>% 
  st_buffer(3218) %>% 
  st_bbox(gg) %>% 
  st_as_sfc() %>% 
  st_as_sf()

Pad <- st_intersection(extent, padus) %>% 
  mutate(Own_Name = if_else(Own_Name == 'CITY_CNTY_SDC_SDNR_SPR', 'Local-State', Own_Name))
bbox <- st_bbox(extent)

public_lands_pal1 <- public_lands_pal
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
plp1 <- '#FECC5C'
names(plp1) <- 'Wilderness'
plp1 <- c(plp, plp1)

ACEC <- st_crop(acec, bbox)
acec_grid <- st_make_grid(ACEC, square = F , flat_topped =  T, n = c(25, 25))
acec_grid <- st_intersection(acec, acec_grid)
acec_grid <- st_crop(acec_grid, bbox)
ADMU <- st_crop(administrative_boundaries, bbox)

hill <- crop(hill, ext(terra::vect(extent))) 
hillshade <- as.data.frame(hill, xy = T)

acec_streams <- st_crop(streams, bbox)
acec_rivers <- st_crop(rivers, bbox)

AIM <- aim %>% mutate(Dumm = 'A') %>% st_crop(., bbox)
mask_de <- st_crop(mask, bbox)

deNCA <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.5) +
  geom_sf(data = wa, fill = '#FECC5C', color = NA, alpha = 0.4) +
  geom_sf(data = acec_grid, fill = NA, aes(color = 'maroon4'), alpha = 0.5) +
  geom_sf(data = de, aes(color = 'darkgreen'), lwd = 1.0, 
          fill = NA, alpha = 0.5) +
  geom_sf(data = acec_rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = acec_streams, alpha = 0.1, color = 'blue', lty = 'twodash') +
  geom_sf(data = AIM, aes(shape = Dumm), size = 3) +
  geom_sf(data = mask_de, color = 'white', alpha = 0.5, lwd = 0) +
  geom_sf(data = ADMU, fill = NA, aes(color = 'black'), lwd = 1.0) +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  labs(title = 'Plots near Dominguez-Escalente NCA') +
  scale_fill_manual('Management', values = plp1) +
  scale_color_identity(name = NULL,  breaks = c("darkgreen", "maroon4", "black"),
                       labels = c("NCA", "ACEC", "UFO/GJFO"),  guide = "legend") +
  scale_shape_manual(name = NULL, labels = 'AIM Plots', values =  18) +
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    shape = guide_legend(order = 3)) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                   width_hint = 0.225) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) 


setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style')
ggsave(deNCA, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'DominguezEscalenteAIMPlots.png',
       dpi = 300)

rm(deNCA, de, plp, plp1, public_lands_pal1, hillshade, mask_de, acec_rivers, acec_streams)

# Map of points sampled across the entire field office

ADMU <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE')
extent <- ADMU %>% 
  st_buffer(1000) %>% 
  st_bbox(gg) %>% 
  st_as_sfc() %>% 
  st_as_sf()
bbox <- st_bbox(extent)

ACEC <- st_crop(acec, bbox)
acec_grid <- st_make_grid(ACEC, square = F , flat_topped =  T, n = c(20, 20))
acec_grid <- st_intersection(ACEC, acec_grid)

hill <- rast(
  file.path(p2carto, raster_data[grep('Hill.*coarse', raster_data)]) )
hill <- crop(hill, ext(terra::vect(extent)))
hillshade <- as.data.frame(hill, xy = T)

streams <- st_crop(streams, bbox)
rivers <- st_crop(rivers, bbox)
mask <- st_crop(mask, bbox)
Pad <- st_crop(padus, bbox)

aim_ufo <- aim %>% mutate(Dumm = 'A') %>% st_crop(., bbox)
nm_and_nca <- st_crop(nm_and_nca, bbox)

public_lands_pal1 <- public_lands_pal
names(public_lands_pal1)[11] <- 'Local-State'
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
plp <- plp[!is.na(plp)]

aim_pts <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(aspect.ratio=1, plot.title = element_text(hjust = 0.5)) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
  geom_sf(data = wa, fill = '#FECC5C', color = NA, alpha = 0.4) +
  geom_sf(data = acec_grid, fill = NA, aes(color = 'maroon4'), alpha = 0.5) +
  geom_sf(data = rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = aim_ufo, aes(shape = Dumm), size = 1) +
  geom_sf(data = ADMU, fill = NA, aes(color = 'black'), lwd = 1.0) +
  geom_sf(data = nm_and_nca, fill = NA, aes(color = 'darkgreen'), lwd = 1.0) +
  geom_sf(data = mask, color = 'white', alpha = 0.7, lwd = 0)  +

  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  theme_void() +
  theme(legend.title.align = 0.5, 
        plot.title = element_text(hjust = 0.5),
        title=element_text(hjust = 0.5)) +
  
  labs(title = 'AIM Plots sampled in the UFO') +
  scale_fill_manual('Management', values = plp) +
  scale_color_identity(name = NULL,  breaks = c("darkgreen", "maroon4", "black"),
                       labels = c("NCA", "ACEC", "UFO"),  guide = "legend") +
  scale_shape_manual(name = NULL, labels = 'AIM Plots', values =  23) +
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    shape = guide_legend(order = 3)) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.35, "in"), pad_y = unit(0.4, "in"),
                   width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.65, "in"),
                         style = north_arrow_minimal) 

aim_pts
ggsave(aim_pts, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'AIMPlots.png',
       dpi = 300, width = 6, height = 9, units = "in")



################################################################################
#######       NE Portion of the field office for Invasive Species       ########
################################################################################

r_locations_ne <- st_read(file.path(p2carto, 'noxious', 'noxious_NE.shp'))
ne_labels <- c(
  "Alyssum desertorum",  "Cardaria chalepensis", "Cardaria draba", 
  "Cirsium arvense", "Convolvulus arvensis",  "Cynoglossum officinale", 
  "Gypsophila elegans", "Lepidium densiflorum",  "Phleum pratense",  
  "Polygonum aviculare",  "Potentilla argentea", "Tamarix ramosissima")

bbox <- st_bbox(
  setNames(c(221114.9, 4278752.0,  286377.4, 4311463.6 ),
           c('xmin', 'ymin', 'xmax', 'ymax' )) )
bbox <- st_bbox(
  setNames(c(bbox['xmin'] - 2500, bbox['ymin'] - 2500, bbox['xmax'] + 2500, 
             bbox['ymax'] + 2500),
           c('xmin', 'ymin', 'xmax', 'ymax' )) 
)

hill <- crop(hill, terra::ext(terra::vect(st_as_sfc(bbox))))
hillshade <- as.data.frame(hill, xy = T)

streams <- st_crop(streams, bbox)
rivers <- st_crop(rivers, bbox)
mask <- st_crop(mask, bbox)
Pad <- st_crop(padus, bbox) %>% 
  filter(!Own_Name %in% c('USBR', 'FWS'))

places <- tigris::places(state = 'CO') %>% 
  vect() %>% 
  project(., crs(streams)) %>% 
  crop(., ext(streams)) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  select(NAME) %>% 
  filter(NAME %in% c('Delta', 'Hotchkiss', 'Cedaredge', 'Paonia', 'Crawford'))

public_lands_pal1 <- public_lands_pal
names(public_lands_pal1)[11] <- 'Local'
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
plp <- plp[!is.na(plp)]

p1 <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(plot.title = element_text(hjust = 1.3),
        legend.key.size = unit(0.7, 'lines'),
        plot.margin = unit(c(0,0,-4,0), "lines"),
        legend.title = element_text(hjust = 0.5)) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
  geom_sf(data = rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = streams, alpha = 0.1, color = 'blue') +
  geom_sf(data = mask, color = 'white', alpha = 0.7, lwd = 0)  +
  geom_sf(data = r_locations_ne, aes(color = SYMBOL), shape = 7, size = 3) +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax']))  +
  
  labs(title = 'Invasive Species in the NE Field Office') +
  scale_fill_manual('Management', guide = 'none', values = plp) +
  scale_color_manual('Species', labels=ne_labels,
                     values = colorspace::qualitative_hcl(12, palette = "Dark 3")) +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.75, label.size  = NA) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.25, "in"), pad_y = unit(0.15, "in"),
                   width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_minimal) 

mleg <- get_legend(
  ggplot() +
    guides(fill = 'none') +
    theme_void() +
    ggnewscale::new_scale_fill() +
    
    geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
    scale_fill_manual('Management   ', values = plp) +
    theme(legend.position = 'bottom', legend.box="vertical", 
          legend.title = element_text(size = 8),
          legend.key.size = unit(0.3, 'cm'), 
          legend.text = element_text(size = 6),
          plot.margin = unit(c(0,0,0,0), "lines"))
)

my_cow <- plot_grid(p1, mleg, 
          ncol = 1, rel_heights = c(.9, .1))

setwd('/media/sagesteppe/ExternalHD/UFO_noxious_weeds')
save_plot(my_cow, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'NothernFO_weeds.png',
       dpi = 300,  base_width = 6, units = "in")

#############################################################################
# Show invasive species in other locations throughout the field office

r_locations <- st_read(file.path(p2carto, 'noxious', 'noxious_all.shp'))
r_locations <- st_jitter(r_locations, amount = 1500)

inv <- read.csv(file.path(p2carto, 'noxious', 'Introducted_species_CO.csv')) %>% 
  select(National_USDASymbol, National_SciName_noAuthority) %>% 
  filter(National_USDASymbol %in% r_locations$SYMBOL) %>% 
  pull(National_SciName_noAuthority)
inv <- unique(inv)

bbox <- st_bbox(r_locations)
bbox <- st_bbox(
  setNames(c(bbox['xmin'] - 2500, bbox['ymin'] - 2500, bbox['xmax'] + 2500, 
             bbox['ymax'] + 2500),
           c('xmin', 'ymin', 'xmax', 'ymax' )) 
  )

hill <- crop(hill, ext(terra::vect(r_locations)))
hill <- aggregate(hill, 10)
hillshade <- as.data.frame(hill, xy = T)

rivers <- st_crop(rivers, bbox)
mask <- st_crop(mask, bbox)
Pad <- st_crop(padus, bbox) %>% 
  filter(!Own_Name %in% c('LOC', 'JNT', 'TRIB', 'DOD', 'USBR', 'FWS'))

places <- tigris::places(state = 'CO') %>% 
  vect() %>% 
  project(., crs(streams)) %>% 
  crop(., ext(streams)) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  select(NAME) %>% 
  filter(NAME %in% c('Nucla', 'Cedaredge', 'Montrose', 'Ridgway'))

public_lands_pal1 <- public_lands_pal
names(public_lands_pal1)[11] <- 'Local-State'
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
plp <- plp[!is.na(plp)]

p1 <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(hjust = 0.5, size = 12), 
        legend.key.size = unit(0.7, 'lines'),
        legend.position = 'right', 
        legend.spacing.y = unit(0.1, 'pt'),
        legend.spacing.x = unit(0.0, 'pt'),
        plot.margin = unit(c(0,-2,0,0), "lines"),
        legend.text = element_text( size = 6,
        margin = margin(l = 0, unit = "pt"))) +
  ggnewscale::new_scale_fill() +
    
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
  geom_sf(data = rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = mask, color = 'white', alpha = 0.7, lwd = 0)  +
  geom_sf(data = r_locations, aes(color = SYMBOL)) + 

  labs(title = 'Invasive Species') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.45, label.size  = NA) +
  scale_fill_manual('Management', guide = 'none', values = plp) +
  scale_color_manual('', 
                     values = colorspace::qualitative_hcl(length(inv) + 1, palette = "Dark 3")) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.2, "in"), pad_y = unit(0.15, "in"),
                   width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.04, "in"), pad_y = unit(2.0, "in"),
                         style = north_arrow_minimal) +
  guides(color = guide_legend(byrow = TRUE))


mleg <- get_legend(
  ggplot() +
    guides(fill = 'none') +
    theme_void() +
    ggnewscale::new_scale_fill() +
    
    geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
    scale_fill_manual('Management   ', values = plp) +
    theme(legend.position = 'bottom', legend.box="vertical", 
          plot.margin = unit(c(0,-2.5,0,0), "lines"),
          legend.text = element_text(size = 9,
                                     margin = margin(l = 1, unit = "pt")))
)


my_cow <- plot_grid(p1, mleg, 
                    ncol = 1, rel_heights = c(.85, .15))

ggsave(my_cow, path = 'results/maps', device = 'png',
          bg = 'transparent', filename = 'allFO_weeds.png',
          dpi = 300, width = 6, units = "in")

################################################################################
#########           Plot  Invasive species aggregate index            ##########
################################################################################


setwd('/media/sagesteppe/ExternalHD/UFO_noxious_weeds')

r_locations <- st_read(file.path(p2carto, 'noxious', 'noxious_index.shp'))
r_locations <- st_jitter(r_locations, amount = 1500)
bbox <- st_bbox(r_locations)
bbox <- st_bbox(
  setNames(c(bbox['xmin'] - 2500, bbox['ymin'] - 2500, bbox['xmax'] + 2500, 
             bbox['ymax'] + 2500),
           c('xmin', 'ymin', 'xmax', 'ymax' )) 
)

hill <- crop(hill,  ext(terra::vect(r_locations)))
hill <- aggregate(hill, 10)
hillshade <- as.data.frame(hill, xy = T)

rivers <- st_crop(rivers, bbox)
mask <- st_crop(mask, bbox)
Pad <- st_crop(padus, bbox) %>% 
  filter(!Own_Name %in% c('LOC', 'JNT', 'TRIB', 'DOD', 'USBR', 'FWS'))

public_lands_pal1 <- public_lands_pal
names(public_lands_pal1)[11] <- 'Local-State'
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
plp <- plp[!is.na(plp)]

places <- tigris::places(state = 'CO') %>% 
  vect() %>% 
  project(., crs(Pad)) %>% 
  crop(., ext(Pad)) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  select(NAME) %>% 
  filter(NAME %in% c('Nucla', 'Cedaredge', 'Montrose', 'Ridgway',
                     'Crawford'))

p1 <- ggplot() +
  geom_histogram(data = r_locations, aes(y = Indx_Pr, fill = ..y..)) + 
  scale_y_continuous('Invasive Index', position = "right", expand = c(0,0)) +
  scale_x_continuous('Plot Count', expand = c(0,0)) +
  colorspace::scale_fill_continuous_divergingx(
    name = 'Value', palette = "RdYlGn", 
    mid = 0.25, rev = T) +
  theme_bw(base_size = 10) +
  theme(aspect.ratio = 16/4.15, 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.margin = unit(c(0,0,0,-2), "lines"),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
        )

p2 <- ggplot() +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.65),
        legend.title = element_text(hjust = 0.5, size = 12), 
        legend.position = 'bottom', 
        legend.key.size = unit(0.5, 'cm'), 
        legend.spacing.y = unit(5, 'pt'),
        legend.spacing.x = unit(5, 'pt'),
        plot.margin = unit(c(0,-2.5,0,0), "lines"),
        legend.text = element_text(size = 9,
                                    margin = margin(l = 1, unit = "pt"))) +
  ggnewscale::new_scale_fill()  +
 
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
  scale_fill_manual('Management', values = plp) +
  
  geom_sf(data = rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = mask, color = 'white', alpha = 0.7, lwd = 0)  +
  
  ggnewscale::new_scale_fill()  +
  geom_sf(data = r_locations, aes(fill = Indx_Pr), shape = 23, size = 2) +
  colorspace::scale_fill_continuous_divergingx(palette = "RdYlGn", mid = 0.4, 
                                               rev = T, guide = 'none') + 
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
             ylim = c(bbox['ymin'], bbox['ymax'])) +

  labs(title = 'Invasive Species Index') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.25, label.size  = NA) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.2, "in"), pad_y = unit(0.15, "in"),
                   width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(2.25, "in"),
                         style = north_arrow_minimal) 

my_cow <- plot_grid(p2, p1, rel_heights = c(1,1),
          ncol = 2, rel_widths = c(.87, .13))

save_plot(my_cow, path = 'results/maps', device = 'png',
          bg = 'transparent', filename = 'invasive_index.png',
          dpi = 300, base_width = 6, units = "in")


################################################################################
################     PLOT FLORISTIC QUALITY INDEX PREDICTIONS     ##############  
################################################################################

setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style')

p2pd <- '/media/sagesteppe/ExternalHD/UFO_Plant_Diversity/results/'
fqi_plots <- st_read(
  file.path(p2pd, 'FQI_values.shp') )


fqi_no_xy <- rast(
  file.path(p2pd, 'predicted_FQI_noXY.tif')
)


bbox <- st_bbox(fqi_plots)
bbox <- st_bbox(
  setNames(c(bbox['xmin'] - 2500, bbox['ymin'] - 2500, bbox['xmax'] + 2500, 
             bbox['ymax'] + 2500),
           c('xmin', 'ymin', 'xmax', 'ymax' )) 
)
st_crs(bbox) <- 26913

padus <- st_crop(padus, bbox)

hill <- crop(hill,  bbox)
hill <- aggregate(hill, 10)
hillshade <- as.data.frame(hill, xy = T)

fqi_noXY_prediction <- crop(fqi_no_xy, bbox)
fqi_pred_df_noXY <- as.data.frame(fqi_noXY_prediction, xy = T)

rivers <- st_crop(rivers, bbox)
mask <- st_crop(mask, bbox)

places <- tigris::places(state = 'CO') %>% 
  vect() %>% 
  project(., crs(fqi_no_xy)) %>% 
  crop(., ext(fqi_no_xy)) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  dplyr::select(NAME) %>% 
  filter(NAME %in% c('Nucla', 'Cedaredge', 'Montrose', 'Ridgway',
                     'Crawford', 'Paonia', 'Olathe'))

CO_roads <- tigris::roads(state = 'CO',
                          county = c('Montrose', 'Delta', 'Ouray', 
                                     'San Miguel', 'Mesa')) %>% 
  filter(RTTYP %in% c('U', 'S')) %>% 
  st_transform(st_crs(fqi_no_xy)) %>% 
  st_crop(., bbox) %>% 
  st_simplify() %>% 
  dplyr::select(geometry) %>% 
  st_cast('LINESTRING') %>% 
  st_as_sfc()

non_ufo_blm <-  st_crop(padus, bbox) %>% 
  filter(Own_Name %in% c( 'BLM')) %>% 
  st_intersection(., mask)

Pad <- st_crop(padus, bbox) %>% 
  filter(!Own_Name %in% c('LOC', 'JNT', 'TRIB', 'DOD', 'USBR', 'FWS', 'BLM')) %>% 
  bind_rows(., non_ufo_blm)

public_lands_pal1 <- public_lands_pal
names(public_lands_pal1)[11] <- 'Local-State'
plp <- public_lands_pal[c(unique(Pad$Own_Name))]
plp <- plp[order(names(plp))]
plp <- plp[!is.na(plp)]

###########             first plot just the points               ###############

fqi_plot <- ggplot() +
  
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5, size = 8), 
        legend.position = 'bottom', 
        legend.key.size = unit(0.5, 'cm'), 
        legend.spacing.y = unit(2, 'pt'),
        legend.spacing.x = unit(5, 'pt'),
        legend.text = element_text(size = 6,
                                   margin = margin(l = 0, unit = "pt"))) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = padus, aes(fill = Own_Name), alpha = 0.7, color = NA) +
  scale_fill_manual('Land Owner', values = plp) +
  ggnewscale::new_scale_fill() +

  geom_sf(data = rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = CO_roads, alpha = 0.5, color = 'black') +
  geom_sf(data = mask, color = 'white', alpha = 0.5, lwd = 0) +
  
  ggnewscale::new_scale_fill() +
  geom_point(data = fqi_plots, aes(fill = mcoc_r, size = fqi_r, 
                                   geometry = geometry),
             stat = "sf_coordinates", shape = 21) +
  scale_fill_viridis_c('Mean C', option = "C",  direction = -1, 
                       limits = c(1,7)) +
  scale_size_binned('FQI') +
  
  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  labs(title = 'Measured Floristic Quality') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.5, label.size  = NA) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.4, "in"), pad_y = unit(0.3, "in"),
                   width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) 


ggsave(fqi_plot, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'FQI-plots.png',
       dpi = 300, width = 6, height = 6, units = "in")

rm(fqi_plot)
##############              now plot the prediction               ##############


fqi_pred <- ggplot() +
  
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), 
              interpolate = F)  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5, size = 8), 
        legend.position = 'bottom', 
        legend.key.size = unit(0.5, 'cm'), 
        legend.spacing.y = unit(2, 'pt'),
        legend.spacing.x = unit(5, 'pt'),
        legend.text = element_text(size = 6,
                                   margin = margin(l = 0, unit = "pt"))) +
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = Pad, aes(fill = Own_Name), alpha = 0.7, color = NA) +
  scale_fill_manual('Land Owner', values = plp) +
  ggnewscale::new_scale_fill() +
  
   geom_raster(data = fqi_pred_df_noXY, aes(x = x, y = y, fill = sum), 
                interpolate = F) +
  scale_fill_viridis_c('Mean C', option = "C",  direction = -1) +
  
  geom_sf(data = rivers, alpha = 0.5, color = 'blue') +
  geom_sf(data = CO_roads, alpha = 0.5, color = 'black') +
  geom_sf(data = mask, color = 'white', alpha = 0.5, lwd = 0) +

  coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), 
           ylim = c(bbox['ymin'], bbox['ymax'])) +
  
  labs(title = 'Predicted Floristic Quality') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.5, label.size  = NA) +
  
  annotation_scale(location = "bl", 
                   pad_x = unit(0.4, "in"), pad_y = unit(0.3, "in"),
                   width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal) 
  
ggsave(fqi_pred, path = 'results/maps', device = 'png',
       bg = 'transparent', filename = 'FQI-prediction.png',
       dpi = 300, width = 6, height = 6, units = "in")

rm(fqi_pred, Pad, plp, places, plp, non_ufo_blm, padus, p2carto, rivers, public_lands_pal,
  bbox, CO_roads, fqi_no_xy, fqi_noXY_prediction, ext_ter, mask, p2pd, hill, fqi_plots,
  public_lands_pal1, raster_data, hillshade, hill, vector_data, fqi_pred_df_noXY, 
  administrative_boundaries)


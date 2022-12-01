library(terra)
library(sf)
library(tidyverse)
library(ggnewscale)

p <- '/media/sagesteppe/ExternalHD/AIM_Field_rasters'
demp <- file.path(p, 'UFO_dem_10_smooth_UTM.tif')
hillshade_p <- file.path(p, 'UFO_Hillshade.tif')

rm(p)
# topographic map for the UFO
pcarto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
coarseDEM <- rast(file.path(pcarto,
                       'EarthEnv-DEM90_N35W110/EarthEnv-DEM90_N35W110.bil' ))

template <- rast(demp)
rm(pcarto)

coarseDEM <- aggregate(coarseDEM, 5, method="bilinear")
coarseDEM <- project(coarseDEM, crs(template))
coarseDEM <- crop(coarseDEM, template)
plot(coarseDEM)

slope <- terrain(coarseDEM, "slope", unit="radians")
aspect <- terrain(coarseDEM, "aspect", unit="radians")
hill <- shade(slope, aspect, 30, 315)
plot(hill, col=grey(0:100/100), legend=FALSE, mar=c(2,2,1,4))
plot(coarseDEM, col=rainbow(25, alpha=0.35), add=TRUE)

altitude <- as.data.frame(coarseDEM, xy = T)
hillshade <- as.data.frame(hill, xy = T)
names(altitude) <- c('x','y','elevation')
altitude$cut <- cut(altitude$elevation, breaks = 25)

p2d <- '/media/sagesteppe/ExternalHD/UFO_cartography'
ifelse(!dir.exists(file.path(p2d, 'Hillshade')), 
       dir.create(file.path(p2d, 'Hillshade')), FALSE)
writeRaster(hill, file.path(p2d, 'Hillshade', 'Hillshade.tif'))
rm(aspect, hill)

places <- tigris::places(state = 'CO') %>% 
  vect() %>% 
  project(., crs(coarseDEM)) %>% 
  crop(., ext(coarseDEM)) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  select(NAME) %>% 
  filter(NAME %in% c('Grand Junction', 'Montrose',
                     'Telluride', 'Nucla', 'Paonia'))

rm(coarseDEM)

hillshade_m <- 
ggplot(data = hillshade, aes(x = x, y = y, fill = lyr1)) +
  geom_raster(interpolate = T)  +
  scale_fill_gradient(low = "grey15", high = "grey100") +
  guides(fill = 'none') +
  theme_void() +
  theme(aspect.ratio=1, plot.title = element_text(hjust = 0.5)) +
  new_scale_fill()

elevation_countour <- 
ggplot(hillshade, aes(x = x, y = y)) +
  geom_raster(data = hillshade, aes(x = x, y = y, fill = lyr1), interpolate = T)  +
  scale_fill_gradient(low = "grey15", high = "grey100") + 
  new_scale_fill() +
  geom_tile(data = altitude, aes(fill = elevation),
            alpha = 0.65, colour = NA) +
  scale_fill_gradientn(colors = rainbow(n = 25)) +
  geom_contour(data = altitude, aes(z = elevation), 
               colour ='black', alpha = 0.65, lty = 1) +
  theme_void() +
  theme(legend.position = 'none',
        aspect.ratio=1,
        plot.title = element_text(hjust = 0.5)) +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.75, label.size  = NA) +
  labs(title = 'Area of Drought Analysis') 


p1 <- '/media/sagesteppe/ExternalHD/UFO_elements_of_style/results/maps'
ggsave(plot = hillshade_m, device = "png", filename = 'hillshade_drought',
       path = p1, 
       width = 3.5, height = 3.5, units = 'in',  dpi = 300)

ggsave(plot = elevation_countour, device = "png", filename = 'elevation_contour_drought.png',
       path = p1, 
       width = 3.5, height = 3.5, units = 'in',  dpi = 300)


rm(demp, hillshade_p, p1, hillshade_m, places, slope, template, 
   altitude, elevation_countour, hillshade)

library(whitebox)
library(terra)
library(sf)
library(tidyverse)
wbt_init()

p <- '/media/sagesteppe/ExternalHD/AIM_Field_rasters'
demp <- file.path(p, 'UFO_dem_10_smooth_UTM.tif')
hillshade_p <- file.path(p, 'UFO_Hillshade.tif')

# topographic map for the UFO
pcarto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
coarseDEM <- rast(file.path(pcarto,
                       'EarthEnv-DEM90_N35W110/EarthEnv-DEM90_N35W110.bil' ))

template <- rast(demp)

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

places <- tigris::places(state = 'CO') %>% 
  vect() %>% 
  project(., crs(coarseDEM)) %>% 
  crop(., ext(coarseDEM)) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  select(NAME) %>% 
  filter(NAME %in% c('Grand Junction', 'Montrose',
                     'Telluride', 'Nucla', 'Paonia'))

ggplot(hillshade, aes(x = x, y = y)) +
  geom_tile(aes(colour = lyr1), lwd = 0) +
  geom_raster(data = altitude, aes(fill = cut),
              alpha = 0.8, interpolate = T) +
  geom_contour(data = altitude, aes(z = elevation), 
               colour ='black', alpha = 0.5, lty = 1) +
  scale_fill_manual(values = rainbow(25)) +
  scale_colour_gradient(low = "grey25", high = "white") +
  theme_void() +
  theme(legend.position = "none",
        aspect.ratio=1,
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Area of Drought Analysis') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.75, label.size  = NA)

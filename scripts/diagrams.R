library(sf)
library(tidyverse)
setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style/scripts')

set.seed(1)

d <- data.frame(
  'y' = sample(3:1000, size = 50),
  'x' = sample(3:1000, size = 50),
  'panel' = paste(rep(LETTERS[1:5], each = 10), '-', rep(2020:2024, each = 10))
)

ggplot(d, aes(x = x, y = y, color = panel,
              shape = panel)) +
  geom_jitter(width = 50, height = 50, size = 2) +
  theme_bw() +
  scale_color_manual(values = 
                       RColorBrewer::brewer.pal(n = 5, name = 'Dark2')) +
  scale_shape_manual('Panel', values=c(15:19)) +
  labs(title = "A sample frame with five panels", color = 'Panel',
       x = 'Longitude (m)', y = 'Latitude (m)') +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    ) +
  lims(x = c(0,1000), y = c(0,1000))

ggsave('../results/plots/SampleFrameExample.png', dpi = 300, height = 10, width = 10
       , units ="cm", 
       device = "png")

rm(d)





sacrifice_zone <- st_point(c(50,50)) %>% 
  st_geometry()  %>% 
  st_as_sf(crs = 26913) %>% 
  st_buffer(5) %>% 
  mutate(Attribute = 'Sacrifice') %>% 
  rename('geometry' = x) %>% 
  st_as_sf()  

plot_outer <- sacrifice_zone %>% 
  st_buffer(25) %>% 
  st_difference(., sacrifice_zone) %>% 
  mutate(Attribute = 'Species Richness', .before = geometry) 

transects <- st_multilinestring(
  list(
#    st_linestring(rbind(c(50, 50), c(50, 80))),
    st_linestring(rbind(c(25.98, 25.98), c(50, 50))),
    st_linestring(rbind(c(75.98, 25.98), c(50, 50)))
  )
)  %>% 
  st_geometry()  %>% 
  st_as_sf(crs = 26913) %>% 
  mutate(Attribute = 'Transects') %>% 
  rename('geometry' = x) %>% 
  st_as_sf()  %>% 
  st_intersection(., plot_outer) %>% 
  st_difference(., sacrifice_zone)

lpi <-  data.frame(
  x = rep(50, times = 50),
  y = seq(from = 55.5, to = 80, by = 0.5)
) %>% 
  st_as_sf(coords = c(x = 'x', y = 'y'), crs = 26913) %>% 
  mutate(Attribute = 'Line-Point Intercept', .before = geometry) 

heights <- data.frame(
  x = rep(49, times = 10),
  y = seq(from = 57.5, to = 80, by = 2.5)
) %>% 
  st_as_sf(coords = c(x = 'x', y = 'y'), crs = 26913) %>% 
  mutate(Attribute = 'Heights', .before = geometry) 

soil_stab <- data.frame(
  x = seq(29.8, (46.46 - 4), length = 6),
  y = seq(29.8, (46.46 - 4), length = 6)
) %>% 
  mutate(y = y - 1.5, 
         x = x + 1.5) %>% 
  st_as_sf(coords = c(x = 'x', y = 'y'), crs = 26913) %>% 
  mutate(Attribute = 'Soil Stability', .before = geometry) 

sh <- c("Heights" = 21, "Line-Point Intercept" = 23, "Soil Stability" = 22)
co <- c("Heights" = '#4D455D', "Line-Point Intercept" = 'red',
        "Soil Stability" = '#7DB9B6')

spp_rich <- data.frame(
  x = 68, 
  y = 53,
  Label = 'Species Richness'
) 


ggplot() +
  geom_sf(data = plot_outer, fill = 'beige') +
  geom_sf(data = transects, aes(color = Attribute)) +
  
  geom_sf(data = lpi, aes(shape = Attribute, fill = Attribute),
          size = 0.7, alpha = 0.4) +
  geom_sf(data = heights, aes(shape = Attribute, fill = Attribute), size = 3) +
  geom_sf(data = soil_stab, aes(shape = Attribute, fill = Attribute), size = 3) +
  theme_void() +
  
  scale_shape_manual('Method:', values = sh) +
  scale_fill_manual('Method:', values = co) +
  labs(title = 'Layout of an AIM plot') + 
  theme(plot.title =  element_text(hjust = 0.5), 
        legend.position = 'bottom') +
  
  geom_label(data = spp_rich, aes(x = x, y = y, label = Label)) +
  scale_colour_manual(name = '', values = 'black', labels = 'Gap Intercept') +

  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in")
  )




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
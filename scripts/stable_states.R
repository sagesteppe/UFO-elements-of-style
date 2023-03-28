
setwd('/media/sagesteppe/ExternalHD/UFO_elements_of_style')

library(tidyverse)
library(rayshader)
library(terra)

set.seed(1972)

surface <- 
  matrix(
    c(0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
      0, -1, -1, -1,  0,  2,  3,  2, -1, -5,  0, 
      0, -1, -2, -4, -1,  0,  5,  2, -1, -5,  0, 
      0, -1, -1, -2, -1,  2,  3,  2,  0, -1,  0, 
      0,  0,  0,  0,  0,  0,  1,  0,  1,  0,  0, 
      0,  2,  3,  1,  0,  0,  0, -1,  0,  1,  0, 
      0,  3,  4,  0, -1, -1, -1, -1,  0,  0,  0, 
      0,  4,  5,  0, -1, -2, -3, -3, -2,  0,  0, 
      0,  0,  0,  1, -1, -2, -3, -5, -3, -1,  0, 
      0, -1, -1,  0,  4, -1, -2, -5, -3, -1,  0,
      0, -4, -4, -1,  3,  1,  0, -2, -1, -1,  0, 
      0, -3, -2, -3, -2, -1,  0,  0, -1,  0,  0, 
      0, -1, -2, -4, -3, -2, -1,  0,  0,  2,  0, 
      0, -1, -1, -1, -1,  0, -4,  0,  0,  2,  0, 
      0,  0,  0,  0,  0,  0, -3, -5,  0,  3,  0
      ), nrow = 15, byrow = T, ncol = 11
) 

surface <- surface + 
  matrix(rnorm(nrow(surface) *  ncol(surface), sd = 0.1), ncol = ncol(surface))

rs <- terra::rast(surface, crs = 'epsg:26913')
rs <- terra::disagg(rs, 10, method='bilinear')
rs <- focal(rs, w=matrix(1, 3, 3), mean)
rs_df <- as.data.frame(rs, xy = T)


seg_df <- data.frame(
  type = c('management', 'disturbance'),
  x = c(1,1),
  y = c(1,1),
  focal_mean = c(0,0)
)


arrow = arrow(length = unit(0.04, "npc"))
              
ggplot(data = rs_df, aes(x = x, y = y, z = focal_mean, fill = focal_mean)) +
  geom_raster(interpolate = TRUE) +
  stat_contour(color = 'grey30', alpha = 0.7, linetype = 4, bins = 7) +
  scale_fill_distiller('Elevation', palette = "RdYlGn", limits = c(-5,5)) +
  theme_void() +
  labs(title = 'Alternative Stable States') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title.align = 0.5, 
        legend.position = 'bottom', 
        legend.box="vertical") +
  
  geom_point(x = 7.5, y = 6.2, size = 4, color = 'purple') +
  # disturbance lines
  geom_curve(x = 7.5, xend = 4.0, y = 6.5, yend = 11.8, linetype = 2, 
             arrow = arrow, curvature = 0.1) + # to upper left
  geom_curve(x = 7.5, xend = 9.6, y = 6.5, yend = 12.5, linetype = 2, 
               arrow = arrow, curvature = 0) +  # to upper right
  geom_curve(x = 7.5, xend = 1.8, y = 6.0, yend =  5.0, linetype = 2, 
               arrow = arrow, curvature = 0.1) + # to lower left 
  
  # Hysteresis lines
  geom_curve(xend = 8.5, x = 9.7, yend = 7, y = 12.5, curvature = -0.9, 
             arrow = arrow) + # from upper right
  geom_curve(xend = 5.5, x = 3.8, yend = 7.2, y = 11.8, curvature = -0.1, 
             arrow = arrow) + # from upper left
  
  geom_curve(xend = 3.2, x = 2.2, yend = 2.8, y = 4.1, curvature = 0.2, 
           arrow = arrow) + # from lower left
  geom_curve(x = 4.2, xend = 6.5, yend = 1.7, y = 2.6, curvature = -0.2, 
             arrow = arrow) + # from center bottom
  geom_curve(xend = 8, x = 7.8, yend = 4.8, y = 1.0, curvature = 0.4, 
           arrow = arrow) + # head back to center 
  
  # add dummy obs for mapping
  geom_line(data = seg_df, aes(linetype = type)) + 
  scale_linetype_manual(values =
                          c("management" = "solid", "disturbance" = "dashed")) +
  guides(linetype = guide_legend(title = "Action"))

ggsave(dpi = 300, 'results/plots/Stable_States.png')


rm(arrow, rs, rs_df, seq_df, surface, cols)

library(sf)
library(tidyverse)



d <- data.frame(
  'y' = sample(1:125, size = 50),
  'x' = sample(1:125, size = 50),
  'panel' = sample(rep(1:5, each = 10))
)

ggplot(d, aes(x = x,y = y, color = as.character(panel))) +
  geom_jitter(width = 20) +
  theme_bw() +
  labs(title = "A sample frame with five panels", color = 'Panel',
       x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()
        )


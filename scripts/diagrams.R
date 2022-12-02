library(sf)
library(tidyverse)



d <- data.frame(
  'y' = sample(1:125, size = 50),
  'x' = sample(1:125, size = 50),
  'panel' = sample(rep(1:5, each = 10))
)


plot(d$x,d$ y, col = factor(d$panel),  
     ylab = "", xlab = "", xaxt='n', yaxt  ="n")



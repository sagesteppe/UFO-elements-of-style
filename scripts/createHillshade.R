library(whitebox)
library(terra)
wbt_init()

p <- '/media/sagesteppe/ExternalHD/AIM_Field_rasters'
demp <- file.path(p, 'UFO_dem_10_smooth_UTM.tif')
hillshade_p <- file.path(p, 'UFO_Hillshade.tif')

wbt_hillshade(demp, hillshade_p,azimuth = 315, altitude = 30, zfactor = 1)

demp <- rast(demp)
hillshade_p <- rast(hillshade_p)

ex <- ob@ptr[["vector"]]

s <- rast(
  nrows = round(dim(hillshade_p)[1]/10,0), 
  ncols = round(dim(hillshade_p)[2]/10,0),
  ext = ext(hillshade_p), 
  crs = crs(demp)
  )

x <- resample(hillshade_p, s, method="cubicspline")

p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')
acec <- st_read(
  file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), quiet = T) %>% 
  vect()

x <- project(x, crs(acec))


test_df <- as.data.frame(x, xy = T)
colnames(test_df) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_gradient(low = "black", high = "white") 

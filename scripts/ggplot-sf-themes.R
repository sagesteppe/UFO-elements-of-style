library(here)
library(sf)

p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

# allotments
allotments <- st_read(
  file.path(p2carto, vector_data[grep('*Grazing*', vector_data)]), quiet = T)

# admin boundaries
administrative_boundaries <- st_read(
  file.path(p2carto, vector_data[grep('*admu*', vector_data)]), quiet = T)

# ownership
padus <- st_read(
  file.path(p2carto, vector_data[grep('PAD.*Combined*', vector_data)]), quiet = T)
  
# NCA's
nm_and_nca <- st_read(
  file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), quiet = T)

# tabeguache
tabeguache <- st_read(
  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)

# sage grouse habitat
grouse <- st_read(
  file.path(p2carto, vector_data[grep('*Grouse*', vector_data)]), quiet = T)

# blm travel lines
gtlf_roads <- st_read(
  file.path(p2carto, vector_data[grep('*GTLF*', vector_data)]), quiet = T)

# acec
acec <- st_read(
  file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), quiet = T)

# rivers 
# AIM Primary sample units
# AIM design stratum
# AIM plots





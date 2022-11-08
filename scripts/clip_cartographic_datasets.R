library(sf)
library(tidyverse)

# nearly all of these datasets occur across the state of colorado, here we 
# reduce their spatial extents to an area within 50mi of the UFO field office

# Import template
p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')

aim <- st_read(
  file.path(p2carto, vector_data[grep('*Plots*', vector_data)]), quiet = T)

acec <- st_read(
  file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), quiet = T)

allotments <- st_read(
  file.path(p2carto, vector_data[grep('*Grazing*', vector_data)]), quiet = T)

administrative_boundaries <- st_read(
  file.path(p2carto, vector_data[grep('*admu*', vector_data)]), quiet = T)

grouse <- st_read(
  file.path(p2carto, vector_data[grep('*Grouse*', vector_data)]), quiet = T)

gtlf_roads <- st_read(
  file.path(p2carto, vector_data[grep('*GTLF*', vector_data)]), quiet = T)

mlra <- st_read(
  file.path(p2carto, vector_data[grep('*MLRA*', vector_data)]), quiet = T)

nm_and_nca <- st_read(
  file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), quiet = T)

padus <- st_read(
  file.path(p2carto, vector_data[grep('PAD.*Fee*', vector_data)]), quiet = T)

streams <- st_read(
  file.path(p2carto, vector_data[grep('*Streams*', vector_data)]), quiet = T)

tabeguache <- st_read(
  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)

wa <- st_read(
  file.path(p2carto, vector_data[grep('*unofficial*', vector_data)]), quiet = T)

wsa <- st_read(
  file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), quiet = T)

# subset datasets for FO

unc_bbox <- administrative_boundaries %>%
  filter(FIELD_O == 'UNCOMPAHGRE') %>% 
  st_buffer(8046) %>% 
  st_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_set_precision(1)

st_intersection(st_transform(unc_bbox, st_crs(aim)), aim) %>% 
  select(PlotKey, PlotID) %>% 
  st_transform(st_crs(unc_bbox)) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*Plots*', vector_data)]), 
           append = F)

ACEC1 <- st_intersection(unc_bbox, acec) %>% 
  select(ACEC_NAME, LUP_NAME, ACEC_ID) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*ACEC*', vector_data)]), 
           append = F)

st_intersection(unc_bbox, administrative_boundaries) %>%
  select(FIELD_OFFICE = ADMU_NAME, DISTRICT_OFFICE = PARENT_NAM) %>% 
  mutate(FIELD_OFFICE = str_remove(FIELD_OFFICE, ' FIELD OFFICE'),
         DISTRICT_OFFICE = str_remove(DISTRICT_OFFICE, ' DISTRICT OFFICE')) %>%  
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(.,
           file.path(p2carto, vector_data[grep('*admu*', vector_data)]), 
           append = F)

st_intersection(unc_bbox, grouse) %>% 
  select(OBJECTID:Status_202) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*Grouse*', vector_data)]),
           append = F)

st_intersection(st_transform(unc_bbox, st_crs(mlra)), mlra) %>% 
  st_transform(26913) %>% 
  select(starts_with('MLRA')) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry')  %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*MLRA', vector_data)]),
           append = F)

st_intersection(unc_bbox, nm_and_nca) %>% 
  filter(NLCS_NAME != 'Canyons of the Ancients National Monument') %>% 
  select(NLCS_NAME, NLCS_TYPE) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), 
           append = F)

padus <- st_intersection(st_transform(unc_bbox, st_crs(padus)), padus) %>% 
  st_transform(26913) %>% 
  select(Own_Type, Own_Name, d_Own_Name, Mang_Type) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') 

padus <- padus %>% 
  mutate(Own_Name = case_when(
    Own_Name %in% c('CITY', 'CNTY', 'SDNR', 'SFW', 'SPR', 'OTHS', 'REG') ~ 'CITY_CNTY_SDC_SDNR_SPR', 
    Own_Name %in% c('NGO','PVT') ~ 'PVT',
    TRUE ~ Own_Name
  )) %>% 
  group_by(Own_Name) %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('PAD.*Fee*', vector_data)]), 
           append = F)

st_intersection(unc_bbox, streams) %>% 
  select(NAME) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*Streams*', vector_data)]), 
           append = F)

st_intersection(unc_bbox, wsa) %>% 
  select(NLCS_NAME) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(.,
    file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), append = F)

st_intersection(st_transform(unc_bbox, st_crs(wa)), wa) %>% 
  st_transform(st_crs(unc_bbox)) %>% 
  select(NLCS_NAME) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(.,
           file.path(p2carto, vector_data[grep('*WSA*', vector_data)]), append = F)

rm(unc_bbox, padus, nm_and_nca, grouse, administrative_boundaries, acec, wa, aim,
   mlra)

# these are clipped to the extent of the field office. 
UFO_ADMU <- filter(administrative_boundaries, FIELD_O == 'UNCOMPAHGRE')

allotments <- st_intersection(UFO_ADMU, allotments) %>%  # PROBLEMS WITH GEOMETRY
  select(ALLOT_NAME, ALLOT_NO) %>% 
  st_as_sf() %>% 
  mutate(tempID = 1:n())

allotments_pt <- allotments %>% 
  st_collection_extract("POINT") %>% 
  st_buffer(1) %>% 
  group_by(ALLOT_NO) %>% 
  mutate(geometry = st_union(geometry)) %>% 
  distinct()
allotments_line <- allotments %>% 
  st_collection_extract("LINESTRING") %>% 
  st_buffer(1) %>% 
  group_by(ALLOT_NO) %>% 
  mutate(geometry = st_union(geometry)) %>% 
  distinct()

allotments %>% 
  filter(!tempID  %in% c(allotments_pt$tempID, allotments_line$tempID)) %>% 
  bind_rows(allotments_pt, allotments_line) %>% 
  st_write(.,
           file.path(p2carto, vector_data[grep('*Grazing*', vector_data)]), 
           append = F)

st_intersection(UFO_ADMU, gtlf_roads) %>% 
  select(PLAN_ALLOW, OHV_ROUTE_, OHV_DSGNTN, ROUTE_PRMR, PLAN_ROUTE, PLAN_ASSET, 
         PLAN_SEASO, PLAN_OHV_R, PLAN_PRMRY, OBSRVE_MOD, OBSRVE_ROU, OBSRVE_SRF, 
         ROUTE_PLAN, GTLF_OWN, NEPA_DOC_N) %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*GTLF*', vector_data)]), 
           append = F)

rm(UFO_ADMU, gtlf_roads, allotments_pt, allotments_line, allotments)

# no spatial operations required

tabeguache %>% 
  select(NLCS_NAME) %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]),
           append = F)


rm(tabeguache, p2carto, vector_data, administrative_boundaries)

  
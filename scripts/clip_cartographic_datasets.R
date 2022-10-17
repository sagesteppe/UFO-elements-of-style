library(here)
library(sf)
library(tidyverse)

# nearly all of these datasets occur across the state of colorado, here we 
# reduce their spatial extents to an area within 50mi of the UFO field office


# Import template
p2carto <- '/media/sagesteppe/ExternalHD/UFO_cartography'
vector_data <- list.files(p2carto, recursive = T, pattern = 'shp$')


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

padus <- st_read(
  file.path(p2carto, vector_data[grep('PAD.*Combined*', vector_data)]), quiet = T)

nm_and_nca <- st_read(
  file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), quiet = T)

tabeguache <- st_read(
  file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]), quiet = T)


# subset datasets for FO

unc_bbox <- administrative_boundaries %>%
  filter(ADMU_NAME == 'UNCOMPAHGRE FIELD OFFICE') %>% 
  st_buffer(8046) %>% 
  st_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_set_precision(1)

st_intersection(unc_bbox, acec) %>% 
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

st_intersection(unc_bbox, nm_and_nca) %>% 
  filter(NLCS_NAME != 'Canyons of the Ancients National Monument') %>% 
  select(NLCS_NAME, NLCS_TYPE) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*NCA*', vector_data)]), 
           append = F)


st_intersection(st_transform(unc_bbox, st_crs(padus)), padus) %>% 
  st_transform(26913) %>% 
  select(Own_Type, Own_Name, d_Own_Name, Loc_Own, Des_Tp, d_Des_Tp) %>% 
  rename(geometry = x) %>% 
  st_set_geometry('geometry') %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('PAD.*Combined*', vector_data)]), 
           append = F)

rm(unc_bbox, padus, nm_and_nca, grouse, administrative_boundaries, acec)

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
allotments <- allotments %>% 
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

rm(UFO_ADMU, gtlf_roads, allotments_pt, allotments_line)

# no spatial operations required

tabeguache %>% 
  select(NLCS_NAME) %>% 
  st_write(., 
           file.path(p2carto, vector_data[grep('*Tabeguache*', vector_data)]),
           append = F)


ggplot(padus) +
  geom_sf()

rm(tabeguache)



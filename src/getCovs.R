library(tidyverse)
library(lubridate)
library(sf)

rm(list = ls())

### Load data and crop to study area -------------------------------------------

grid.path <- "data/L9310x10grid.rds"
L93_grid <- readRDS(grid.path)%>%
  st_as_sf(crs = 2154)

# https://www.hydrosheds.org/products
# rivers.Rawpath <- "data/HydroRIVERS_v10_eu_shp/"
# lakes.Rawpath <- "data/HydroLAKES_polys_v10_shp/"
# 
# rivers <- read_sf(rivers.Rawpath) %>% 
#   st_transform(crs = 2154) %>%
#   st_intersection(st_union(L93_grid))
# 
# lakes <- read_sf(lakes.Rawpath) %>% 
#   st_transform(crs = 2154) %>%
#   st_intersection(st_union(L93_grid))
# 
# saveRDS(lakes, lakes.path)
# saveRDS(rivers, rivers.path)

rivers.path <- "data/rivers.rds"

rivers <- readRDS(rivers.path)%>%
  filter(ORD_STRA > 1) %>%
  st_as_sf(crs = 2154)

### Extract hydrological information -------------------------------------------

L93_grid <- st_intersection(rivers, L93_grid) %>%
  mutate(hydroLen = st_length(geometry),
         ripBuffer = st_area(st_buffer(geometry, 100))) %>%
  st_drop_geometry() %>%
  group_by(gridCell) %>%
  summarize(hydroLen = sum(hydroLen),
            ripBuffer = sum(ripBuffer)) %>% 
  left_join(L93_grid, .) %>% 
  mutate(hydroLen = ifelse(is.na(hydroLen), 0, hydroLen))

### Land use -------------------------------------------------------------------

filenames <- list.files("data/CLC", full.names = TRUE, recursive = TRUE, pattern = "CLC12_")
filenames <- filenames[grep(".shp", filenames)]

CLC_dat <- map(filenames, read_sf) %>%
  reduce(rbind)
 
riparian_habitats <- CLC_dat %>%
  filter(CODE_12 %in% c("311", "312", "313", "321", "322", "323")) %>%
  st_intersection(st_buffer(rivers, 100))

rm(CLC_dat)

L93_grid <- st_intersection(riparian_habitats, L93_grid) %>%
  mutate(ripArea = st_area(geometry)) %>%
  st_drop_geometry() %>%
  group_by(gridCell) %>%
  summarize(ripArea = sum(ripArea)) %>% 
  left_join(L93_grid, .) %>% 
  mutate(ripArea = ifelse(is.na(ripArea), 0, ripArea))

L93_grid <- L93_grid %>% 
  mutate(ripProp = as.numeric(ripArea / ripBuffer))%>% 
  mutate(ripProp = ifelse(is.na(ripProp), 0, ripProp))

### Plot -----------------------------------------------------------------------

ggplot(L93_grid)+
  geom_sf(aes(fill = ripProp), col = NA) +
  scale_fill_gradient(low = "#003300", high ="chartreuse4") +
  theme_bw()

ggplot(L93_grid)+
  geom_sf(aes(fill = hydroLen), col = NA)+
  theme_bw()

## Save ------------------------------------------------------------------------

saveRDS(L93_grid, "data/L9310x10grid_covs.rds")

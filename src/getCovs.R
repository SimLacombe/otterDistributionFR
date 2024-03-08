library(tidyverse)
library(lubridate)
library(sf)

rm(list = ls())

### Load data and crop to study area -------------------------------------------

# https://www.hydrosheds.org/products
rivers.Rawpath <- "data/HydroRIVERS_v10_eu_shp/"
lakes.Rawpath <- "data/HydroLAKES_polys_v10_shp/"
rivers.path <- "data/rivers.rds"
lakes.path <- "data/lakes.rds"
grid.path <- "data/L9310x10grid.rds"

L93_grid <- readRDS(grid.path)%>%
  st_as_sf(crs = 2154)
lakes <- readRDS(lakes.path)%>% 
  st_as_sf(crs = 2154)
rivers <- readRDS(rivers.path)%>% 
  st_as_sf(crs = 2154)

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


### Extract hydrological information -------------------------------------------

hydroCovs <- st_intersection(rivers, L93_grid) %>%
  mutate(hydroLen = st_length(geometry)) %>%
  st_drop_geometry() %>%
  group_by(grid.cell) %>%
  summarize(hydroLen = sum(hydroLen),
            maxDis = max(DIS_AV_CMS))
### Extract lake information ---------------------------------------------------

lakeCovs <- st_intersection(lakes, L93_grid) %>%
  mutate(hydroLen = st_area(geometry)) %>%
  st_drop_geometry() %>%
  group_by(grid.cell) %>%
  summarize(waterBodySurface = sum(hydroLen)) 

### Merge with grid dataframe --------------------------------------------------

L93_grid <- L93_grid %>% 
  left_join(hydroCovs) %>% 
  left_join(lakeCovs)%>%
  mutate(waterBodySurface = ifelse(is.na(waterBodySurface), 0, waterBodySurface),
         hydroLen = ifelse(is.na(hydroLen), 0, hydroLen),
         maxDis = ifelse(is.na(maxDis), 0, maxDis),
         logmaxDis = log(maxDis + 0.001),
         logWBSurf = log(waterBodySurface + 0.001))

### Plot -----------------------------------------------------------------------

plot.dis <- ggplot(L93_grid)+
  geom_sf(aes(fill = logmaxDis))+
  theme_bw()

plot.len <- ggplot(L93_grid)+
  geom_sf(aes(fill = hydroLen))+
  theme_bw()

plot.lakes <- ggplot(L93_grid)+
  geom_sf(aes(fill = logWBSurf))+
  theme_bw()

ggpubr::ggarrange(plot.dis, plot.len, plot.lakes, nrow = 1)


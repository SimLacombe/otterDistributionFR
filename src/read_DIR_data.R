library(tidyverse)
library(lubridate)
library(sf)

rm(list = ls())

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Load Map -------------------------------------------------------------------
map_FR <- readRDS("data/map_fr.rds")%>%
  st_as_sf(crs = 2154)

### Load data ------------------------------------------------------------------

DIRData.path <- "data/Données_collision_DIR"
otterData.path <- "data/otterDatFiltered.rds"

DirData <- read_sf(DIRData.path) %>%
  st_transform(crs = 2154) %>%
  mutate(datatype = "DIR") %>%
  select(year = annee, datatype)

otterData <- readRDS(otterData.path) %>% 
  filter(collision) %>%
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)%>%
  mutate(datatype = "AUTRE") %>%
  select(year, datatype)

roadData <- rbind(DirData, otterData)

ggplot(map_FR) + 
  geom_sf() + 
  geom_sf(data = roadData, aes(col = datatype), alpha = 0.8)+
  facet_wrap(~year)+
  theme_bw()

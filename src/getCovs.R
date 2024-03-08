library(tidyverse)
library(lubridate)
library(sf)

rm(list = ls())

# https://geo.data.gouv.fr/fr/datasets/ae65b462bf55e52fcbbdf1c342fcff12353d78c6
river.path <- "data/SWB_RW_L93/"
map.path <- "data/map_fr.rds"

map <- readRDS(map.path)%>% 
  st_as_sf(crs = 2154)
river.nw <- read_sf(river.path) %>%
  st_intersection(st_union(map))

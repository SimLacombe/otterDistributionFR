library(tidyverse)
library(foreach)
library(sf)
library(SpatialKDE)

rm(list = ls())

source("src/utility_functions.R")

tmp.res = 4

### Load data ###

data.filename <- "data/otterDat.rds"
map.filename <- "data/map_fr.rds"
grid.filename <- "data/L9310x10grid.rds"

otterDat <- readRDS(data.filename) %>% 
  filter(year %in% 2009:2023, !is.na(date)) %>% 
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)

otterDat$period = otterDat$year %/% tmp.res

PO.dat <- otterDat %>%
  filter(!PNA.protocole)

map <- readRDS(map.filename) %>%
  st_union()

L93_grid <- readRDS(grid.filename) %>%
  st_intersection(map) %>%
  dplyr::select(grid.cell)

### Get KDE ###
timesteps <- unique(otterDat$period)

grid.kde <- foreach(timestep = timesteps, .combine = rbind) %do% {
  
  kde(PO.dat %>% filter(period == timestep),
      band_width = 35000, grid = L93_grid) %>% 
    mutate(period = timestep)
}

ggplot(grid.kde) + 
  geom_sf(aes(fill = log(kde_value)))+
  geom_sf(data = PO.dat, aes(col = factor(presence)), pch = 4, alpha = 0.5)+
  scale_color_manual(values = c("red", "black"))+
  facet_wrap(~paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))


# saveRDS(grid.kde, "data/L9310x10grid_KDE.rds")

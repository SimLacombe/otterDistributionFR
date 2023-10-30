library(tidyverse)
library(sf)
library(concom)

source("src/utility_functions.R")

### Load data ###

data.filename <- "data/otterDat.rds"

otterDat <- readRDS(data.filename) %>% 
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)

### Keep only PACA and PNA standard data for now ###

otterDat <- otterDat %>% 
  filter(region == "PACA")

### Get individual transects ### 

thr.space <- 500
thr.time <- 2

otterDat <- otterDat %>% 
  group_by(PNA.protocole, year, grid.cell) %>%
  mutate(obs = collapse_transects(date, geometry, thr.space, thr.time)) %>% 
  group_by(PNA.protocole, year, grid.cell, obs) %>%
  arrange(desc(presence)) %>%
  filter(row_number()==1) %>% 
  arrange(year, grid.cell) %>%
  filter(obs < 6)

### Format data for occ. model ###


### Plot ###

map.filename <- "data/map_fr.rds"
grid.filename <- "data/L9310x10grid.rds"

map <- readRDS(map.filename) %>%
  filter(code_insee == "93")
L93_grid <- readRDS(grid.filename) %>%
  st_intersection(map)

riv_nw <- read_sf("data/eu_riv_30s/") %>%
  st_transform(crs = 2154) %>%
  st_intersection(map)

otter_naiveOcc <- otterDat %>%
  as.data.frame %>%
  filter(PNA.protocole) %>%
  select(-geometry) %>%
  group_by(data.provider, region, PNA.protocole, year, grid.cell)%>%
  summarize(presence = sign(sum(presence)),
            n.replicates = n()) %>%
  left_join(L93_grid[c("grid.cell", "geometry")], by = "grid.cell") %>%
  st_as_sf(crs = 2154)

ggplot(otterDat)+
  geom_sf(data = map)+
  geom_sf(data = L93_grid, fill = "lightgrey")+
  geom_sf(data = riv_nw, aes(alpha = log(UP_CELLS)), color = "#002266", show.legend = FALSE)+
  geom_sf(data = otter_naiveOcc, aes(fill=factor(presence)), alpha = 0.5) +
  geom_sf(aes(color = factor(presence), pch = PNA.protocole))+
  scale_color_manual(values = c("red", "blue"))+
  scale_shape_manual(values = c(4,19))+
  facet_wrap(~year)+
  theme_bw()

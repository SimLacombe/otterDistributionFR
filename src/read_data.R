library(tidyverse)
library(lubridate)
library(foreach)
library(sf)
library(concom)

rm(list = ls())

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CREATE GRID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

# map_FR <- read_sf("data/regions-20140306-5m-shp/") %>%
#   filter(code_insee %in% c("42", "72", "83", "25", "26", "53",
#                             "24", "21", "43", "23", "11", "91",
#                             "74", "41", "73", "31", "52", "22",
#                             "54", "93", "82")) %>%
#   rmapshaper::ms_simplify() %>%
#   st_transform(crs = 2154)
# 
# insee_to_dataRegion <- data.frame(code_insee = c("42", "72", "83", "25", "26", "53",
#                                                  "24", "21", "43", "23", "11", "91",
#                                                  "74", "41", "73", "31", "52", "22",
#                                                  "54", "93", "82"),
#                                   data_region = c("noDat", "Aq", "Au", "No", "Bo", "Br", "Cvl",
#                                                   "noDat", "FC", "No", "noDat", "Oc", "Li", "noDat", "Oc",
#                                                   "noDat", "PdL", "noDat", "PoCha", "PACA", "RA"))
# 
# map_FR <- map_FR %>%
#   left_join(insee_to_dataRegion, by = "code_insee") %>%
#   group_by(data_region) %>%
#   summarize()
# 
# grid <- st_make_grid(map_FR, crs = 2154,
#                      cellsize = c(10000,10000),
#                      offset = c(99000, 6130000)) %>%
#   st_as_sf
# 
# grid <- cbind(grid, st_coordinates(st_centroid(grid))) %>%
#   rename(geometry = x,
#          lon = X, lat = Y) %>%
#   mutate(gridCell = ifelse(lon >= 1000000,
#                            paste0("E", substr(lon,1,3),"N",substr(lat,1,3)),
#                            paste0("E0", substr(lon,1,2),"N",substr(lat,1,3)))) %>%
#   st_join(map_FR[, "data_region"], largest = TRUE) %>%
#   filter(!is.na(data_region))
# 
# grid2 <- grid %>%
#   st_intersection(st_union(map_FR))
# 
# saveRDS(grid, "data/L9310x10grid_uncropped.rds")
# saveRDS(grid2, "data/L9310x10grid.rds")
# saveRDS(map_FR, "data/map_fr.rds")

grid <- readRDS("data/L9310x10grid_uncropped.rds") %>%
  st_as_sf(crs = 2154)
grid2 <- readRDS("data/L9310x10grid.rds") %>%
  st_as_sf(crs = 2154)
map_FR <- readRDS("data/map_fr.rds") %>%
  st_as_sf(crs = 2154)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Load data ------------------------------------------------------------------

src.path <- "src/open_datafiles"

otterDat <-
  foreach(src = list.files(src.path, full.names = TRUE),
          .combine = rbind) %do% {
            source(src)
            rm(list = setdiff(ls(), c("dat", "src.path", "grid", "grid2", "map_FR")))
            dat
          }

rm(dat)

otterDat <-  filter(otterDat,!is.na(date))

### Get approx lat/lon for obs with missing coordinatess -----------------------

otterDat[is.na(otterDat$lat), c("lon", "lat")] <-
  otterDat %>%
  filter(is.na(lat)) %>%
  select("gridCell") %>%
  left_join(grid[, c("lon", "lat", "gridCell")], by = "gridCell") %>%
  select(lon, lat)

### To spatial object ----------------------------------------------------------

otterDat <-
  st_as_sf(otterDat,
           coords = c("lon", "lat"),
           crs = 2154)

### Get Administrative regions -------------------------------------------------

otterDat <-  st_join(otterDat, grid[, "data_region"])

### Remove redundant observations ----------------------------------------------

otterDat <- filter(otterDat, year %in% 2009:2023,
                    protocol != "PO"|presence)

# source("src/utility_functions.R")
# 
# thr.space = 500
# thr.time = 2
#
# otterDat.filtered.1 <- filter(otterDat, !PA)
#
# otterDat.filtered.2 <- otterDat %>%
#   filter(PA) %>%
#   group_by(year, grid.cell) %>%
#   mutate(obs = collapse_transects(date, geometry, thr.space, thr.time)) %>%
#   group_by(year, grid.cell, obs) %>%
#   arrange(desc(presence)) %>%
#   filter(row_number()==1) %>%
#   arrange(year, grid.cell) %>%
#   filter(obs < 6) %>%
#   select(-obs)
#
# otterDat.filtered <- rbind(otterDat.filtered.1, otterDat.filtered.2)

### SAVE -----------------------------------------------------------------------

saveRDS(otterDat, "data/otterDat.rds")
# saveRDS(otterDat.filtered, "data/otterDatFiltered.rds")

# otterDat.filtered <- readRDS("data/otterDatFiltered.rds") %>%
#   st_as_sf(crs = 2154)
# otterDat <- readRDS("data/otterDat.rds") %>%
#   st_as_sf(crs = 2154)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
otterDat_po <- otterDat %>%
  filter(protocol == "PO") %>%
  mutate(period = year %/% 4)

otterDat_pa <- otterDat %>%
  filter(protocol != "PO")%>%
  mutate(period = year %/% 4)

otterDat_pa %>%
  st_drop_geometry() %>%
  group_by(period, gridCell) %>%
  summarize(presence = any(as.logical(presence)),
            nsample = n()) %>%
  left_join(grid[, c("geometry", "gridCell")], by = "gridCell") %>%
  st_as_sf %>%
  ggplot() +
  geom_sf(data = map_FR) +
  geom_sf(aes(fill = presence)) +
  geom_sf(data = otterDat_po, size = .25) +
  scale_fill_manual(name = "",
                    values = c("orange", "darkblue")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ paste0(period * 4, " - ", period * 4 + 3))

ggplot(otterDat_pa)+
  geom_sf(data = map_FR) +
  geom_sf(aes(col = protocol))

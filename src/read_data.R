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
#          lon.l93 = X, lat.l93 = Y) %>%
#   mutate(grid.cell = ifelse(lon.l93 >= 1000000,
#                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
#                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>%
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

otter.dat <-
  foreach(src = list.files(src.path, full.names = TRUE),
          .combine = rbind) %do% {
            source(src)
            rm(list = setdiff(ls(), c("dat", "src.path", "grid", "grid2", "map_FR")))
            dat
          }

rm(dat)

otter.dat <-  filter(otter.dat,!is.na(date))

### Get approx lat/lon for obs with missing coordinatess -----------------------

otter.dat[is.na(otter.dat$lat.l93), c("lon.l93", "lat.l93")] <-
  otter.dat %>%
  filter(is.na(lat.l93)) %>%
  select("grid.cell") %>%
  left_join(grid[, c("lon.l93", "lat.l93", "grid.cell")], by = "grid.cell") %>%
  select(lon.l93, lat.l93)

## Remove collision data associated to presence-absence ------------------------

otter.dat <- otter.dat %>%
  mutate(
    PA = ifelse(collision, FALSE, PA),
    PA.protocole = ifelse(collision, NA, PA.protocole)
  )

### To spatial object ----------------------------------------------------------

otter.dat <-
  st_as_sf(otter.dat,
           coords = c("lon.l93", "lat.l93"),
           crs = 2154)

### Get Administrative regions -------------------------------------------------

otter.dat <-  st_join(otter.dat, grid[, "data_region"])

### Remove redundant observations ----------------------------------------------

source("src/utility_functions.R")

thr.space = 500
thr.time = 2

otter.dat <- filter(otter.dat, year %in% 2009:2023)

# otter.dat.filtered.1 <- filter(otter.dat, !PA)
#
# otter.dat.filtered.2 <- otter.dat %>%
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
# otter.dat.filtered <- rbind(otter.dat.filtered.1, otter.dat.filtered.2)

### SAVE -----------------------------------------------------------------------

saveRDS(otter.dat, "data/otterDat.rds")
# saveRDS(otter.dat.filtered, "data/otterDatFiltered.rds")

# otter.dat.filtered <- readRDS("data/otterDatFiltered.rds") %>%
#   st_as_sf(crs = 2154)
# otter.dat <- readRDS("data/otterDat.rds") %>%
#   st_as_sf(crs = 2154)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

otter.dat %>%
  st_drop_geometry() %>%
  filter(PA | presence) %>%
  group_by(period = year %/% 4, grid.cell) %>%
  summarize(cell.status = ifelse(
    any(PA & presence > 0),
    "Présente - protocolé",
    ifelse(any(PA), "Non observée - protocolé",
           "Présente - opportuniste")
  ),
  nsample = n()) %>%
  left_join(grid[, c("geometry", "grid.cell")], by = "grid.cell") %>%
  st_as_sf %>%
  ggplot() +
  geom_sf(data = map_FR) +
  geom_sf(
    data = otter.dat %>% filter(!PA & !presence) %>%
      mutate(period = year %/% 4),
    alpha = 0.1
  ) +
  geom_sf(aes(fill = cell.status)) +
  scale_fill_manual(name = "",
                    values = c("orange", "lightblue", "darkblue")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ paste0(period * 4, " - ", period * 4 + 3))

otter.dat %>%
  st_drop_geometry() %>%
  filter(PA) %>%
  group_by(period = year %/% 4, grid.cell) %>%
  left_join(grid[, c("geometry", "grid.cell")], by = "grid.cell") %>%
  st_as_sf %>%
  ggplot() +
  geom_sf(data = map_FR) +
  geom_sf(aes(fill = PA.protocole)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ paste0(period * 4, " - ", period * 4 + 3))

otter.dat %>%
  filter(!PA & presence) %>%
  mutate(period = year %/% 4) %>%
  ggplot() +
  geom_sf(data = map_FR) +
  geom_sf() +
  facet_wrap( ~ period)

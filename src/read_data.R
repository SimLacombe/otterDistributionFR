library(tidyverse)
library(lubridate)
library(foreach)
library(sf)
library(concom)

rm(list = ls())

source("src/functions/getReplicates.R")

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CREATE GRID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# 
# map_FR <- read_sf("data/regions-20140306-5m-shp/") %>%
#   filter(code_insee %in% c("42", "72", "83", "25", "26", "53",
#                             "24", "21", "43", "23", "11", "91",
#                             "74", "41", "73", "31", "52", "22",
#                             "54", "93", "82")) %>%
#   rmapshaper::ms_simplify() %>%
#   st_transform(crs = 2154)
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
#   st_join(map_FR[, "code_insee"], largest = TRUE) %>%
#   filter(!is.na(code_insee))
# 
# grid2 <- grid %>%
#   st_intersection(st_union(map_FR))
# 
# grid2$code_insee <- as.factor(grid2$code_insee)
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

envt <- c(ls(), "otterDat")

src.path <- "src/open_datafiles"

otterDat <-
  foreach(src = list.files(src.path, full.names = TRUE),
          .combine = rbind) %do% {
            source(src)
            dat
          }

rm(list = setdiff(ls(), envt))

otterDat <-  filter(otterDat,!is.na(date),
                    year %in% 2009:2023)

### Get approx lat/lon for obs with missing coordinatess -----------------------

otterDat[is.na(otterDat$lat), c("lon", "lat")] <-
  otterDat %>%
  filter(is.na(lat)) %>%
  select("gridCell") %>%
  left_join(grid[, c("lon", "lat", "gridCell")], by = "gridCell") %>%
  select(lon, lat)

### Get Administrative regions -------------------------------------------------

otterDat <-  left_join(otterDat, grid[, c("gridCell", "code_insee")]) %>%
  select(-geometry)

### Indicate when observer name is missing -------------------------------------

otterDat <- otterDat %>% 
  mutate(observer = ifelse(grepl("trans.|Anonyme", observer)|observer == ""|is.na(observer),
                           "unknown",
                           observer))

### Get sampling areas ---------------------------------------------------------

datPO <-  otterDat %>%
  filter(protocol == "PO")

actionAreas <- datPO  %>%
  group_by(dataSource) %>% 
  summarize(aa = getSamplingArea(lon, lat, lvl = 0.001))

actionAreas <- datPO %>% 
  group_by(year, dataSource) %>%
  summarize() %>% 
  left_join(actionAreas, by = "dataSource") %>%
  st_as_sf(crs = 2154) 

actionAreas <- actionAreas%>%
  arrange(year, st_area(actionAreas))

effortMat <- map(2009:2023, function(x){
  st_join(grid, actionAreas %>% filter(year == x)) %>% 
  st_drop_geometry() %>%
  group_by(gridCell) %>%
  summarize(dataSource = dataSource) %>%
  .$dataSource
}) 

effortMat <- effortMat%>%
  reduce(rbind) %>%
  t

### Remove opportunistic negative data -----------------------------------------

otterDat <- filter(otterDat, protocol != "PO"|presence)

### filter redundant observations ----------------------------------------------

# We keep only spatial replicates for the PA dataset
# For the PO dataset, we keep one observation per day per sampling site

## 1. get site index for each observation 
# (site = sampling unit : points are less than 500 m appart)

otterDat <- otterDat %>%
  group_by(protocol, gridCell, year) %>%
  mutate(site = getSites(lon, lat, thr = 500))

## 2. keep one obs per site per day 
# (present if at least one observation is made)

otterDat <- otterDat %>%
  group_by(protocol, gridCell, year, site, date) %>%
  arrange(desc(presence)) %>%
  filter(row_number()==1|dataSource %in% c("SHNA-OFAB", "LPO-BFC"))

## 3. keep only one visit per site per year
# (we randomly keep one day of observation)

otterDat_pa <- otterDat %>%
  filter(protocol != "PO",
         !dataSource %in% c("SHNA-OFAB", "LPO-BFC"))
otterDat_po <- otterDat %>%
  filter(protocol == "PO",
         !dataSource %in% c("SHNA-OFAB", "LPO-BFC"))
otterDat_BFC <- otterDat %>%
  filter(dataSource %in% c("SHNA-OFAB", "LPO-BFC"))

otterDat_pa <- otterDat_pa %>%
  group_by(protocol, gridCell, year, site) %>%
  slice_sample(n = 1) %>%
  ungroup

otterDat <- rbind(otterDat_pa, otterDat_BFC, otterDat_po) %>%
  select(code_insee, dataSource, observer, protocol, year, date, presence, lon, lat, gridCell) %>% 
  arrange(dataSource, protocol, year, date)

rm(otterDat_pa, otterDat_po, otterDat_BFC)

### SAVE -----------------------------------------------------------------------

saveRDS(otterDat, "data/otterDat.rds")
saveRDS(effortMat, "data/samplingEffort.rds")

# otterDat <- readRDS("data/otterDat.rds")
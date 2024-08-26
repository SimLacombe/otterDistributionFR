library(tidyverse)
library(purrr)
library(sf)
library(concom)

rm(list = ls())

source("src/functions/getReplicates.R")

grid <- readRDS("data/landscape.rds") %>%
  st_as_sf(crs = 2154)

map_FR <- st_union(grid)

surveys <- list(transect = "IUCN",
                pointwise = character(0))

### Load data ------------------------------------------------------------------

envt <- c(ls(), "otterDat")

src.path <- "src/open_datafiles"

otterDat <- map(list.files(src.path, full.names = TRUE), function(path){
  source(path)
  dat
  })

otterDat <- reduce(otterDat, rbind)

rm(list = setdiff(ls(), envt))

otterDat <-  filter(otterDat,!is.na(date),
                    year %in% 2009:2023)

### Change the protocol column to PO, transect or punctual ---------------------

otterDat <- otterDat %>%
  mutate(protocol = ifelse(protocol %in% surveys$transect, "transect",
                           ifelse(protocol %in% surveys$pointwise, "pointwise", "PO")))

### Get approx lat/lon for obs with missing coordinatess -----------------------

otterDat[is.na(otterDat$lat), c("lon", "lat")] <-
  otterDat %>%
  filter(is.na(lat)) %>%
  select("gridCell") %>%
  left_join(grid[, c("lon", "lat", "gridCell")], by = "gridCell") %>%
  select(lon, lat)

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
  mutate(dataSource = dataSource[1]) %>%  
  select(gridCell, dataSource) %>%
  unique %>%
  .$dataSource
  
## Note: the mutate/select/unique sequence is equivalent to summarize but
## does not reorder cols based on the grouping column
}) 

effortMat <- effortMat%>%
  reduce(rbind) %>%
  t

### Remove opportunistic negative data -----------------------------------------

otterDat <- filter(otterDat, protocol != "PO"|presence)

### filter redundant observations ----------------------------------------------

otterDat_pa <- otterDat %>%
  filter(protocol != "PO",
         !dataSource %in% c("SHNA-OFAB", "LPO-BFC"))

otterDat_po <- otterDat %>%
  filter(protocol == "PO"|dataSource %in% c("SHNA-OFAB", "LPO-BFC"))

## 1. We keep only spatial replicates for the PA dataset
## a) get site index for each observation 
# (site = sampling unit : points are less than 500 m appart)

otterDat_pa <- otterDat_pa %>%
  group_by(protocol, gridCell, year) %>%
  mutate(site = getSites(lon, lat, thr = 500))

## b) keep one obs per site per day 
# (present if at least one observation is made)

otterDat_pa <- otterDat_pa %>%
  group_by(protocol, gridCell, year, site, date) %>%
  arrange(desc(presence)) %>%
  filter(row_number()==1|dataSource %in% c("SHNA-OFAB", "LPO-BFC"))

## c) keep only one visit per site per year
# (we randomly keep one day of observation)

otterDat_pa <- otterDat_pa %>%
  group_by(protocol, gridCell, year, site) %>%
  slice_sample(n = 1) %>%
  ungroup %>%
  select(-site)

## 2. Subsample the PO dataset

otterDat_po <- map(unique(otterDat$year), function(yr){
  
  tmp <- filter(otterDat_po, year == yr) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 2154, remove = FALSE)
  
  tmp[subsample(tmp$geometry, thr = 10 * sqrt(2)), ] %>%
    st_drop_geometry
  
}) %>% 
  reduce(rbind)

otterDat <- rbind(otterDat_pa, otterDat_po) %>%
  select(dataSource, observer, protocol, year, date, presence, lon, lat, gridCell) %>% 
  arrange(dataSource, protocol, year, date)

rm(otterDat_pa, otterDat_po)

### SAVE -----------------------------------------------------------------------

saveRDS(otterDat, "data/otterDat.rds")
saveRDS(effortMat, "data/samplingEffort.rds")

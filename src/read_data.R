library(tidyverse)
library(lubridate)
library(foreach)
library(sf)
library(concom)

rm(list = ls())

source("src/functions/getReplicates.R")

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CREATE GRID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

# map_FR <- read_sf("data/regions-20140306-5m-shp/") %>%
#   filter(code_insee %in% c("42", "72", "83", "25", "26", "53",
#                             "24", "21", "43", "23", "11", "91",
#                             "74", "41", "73", "31", "52", "22",
#                             "54", "93", "82")) %>%
#   rmapshaper::ms_simplify() %>%
#   st_transform(crs = 2154)
# 
# insee_to_region <- data.frame(code_insee = c("42", "72", "83", "25", "26", "53",
#                                                  "24", "21", "43", "23", "11", "91",
#                                                  "74", "41", "73", "31", "52", "22",
#                                                  "54", "93", "82"),
#                                   region = c("noDat", "Aq", "Au", "No", "Bo", "Br", "Cvl",
#                                                   "noDat", "FC", "No", "noDat", "Oc", "Li", "noDat", "Oc",
#                                                   "noDat", "PdL", "noDat", "PoCha", "PACA", "RA"))
# 
# map_FR <- map_FR %>%
#   left_join(insee_to_region, by = "code_insee") %>%
#   group_by(region) %>%
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
#   st_join(map_FR[, "region"], largest = TRUE) %>%
#   filter(!is.na(region))
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

envt <- c(ls(), "otterDat")

src.path <- "src/open_datafiles"

otterDat <-
  foreach(src = list.files(src.path, full.names = TRUE),
          .combine = rbind) %do% {
            source(src)
            dat
          }

rm(list = setdiff(ls(), envt))

otterDat <-  filter(otterDat,!is.na(date))

### Get only required observations ---------------------------------------------

otterDat <- filter(otterDat, year %in% 2009:2023,
                   protocol != "PO"|presence)

### Get approx lat/lon for obs with missing coordinatess -----------------------

otterDat[is.na(otterDat$lat), c("lon", "lat")] <-
  otterDat %>%
  filter(is.na(lat)) %>%
  select("gridCell") %>%
  left_join(grid[, c("lon", "lat", "gridCell")], by = "gridCell") %>%
  select(lon, lat)

### Get Administrative regions -------------------------------------------------

otterDat <-  left_join(otterDat, grid[, c("gridCell", "region")]) %>%
  select(-geometry)

### Indicate when observer name is missing -------------------------------------

otterDat <- otterDat %>% 
  mutate(observer = ifelse(grepl("trans.|Anonyme", observer)|observer == ""|is.na(observer),
                           "unknown",
                           observer))
  

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
  select(region, dataSource, observer, protocol, year, date, presence, lon, lat, gridCell) %>% 
  arrange(dataSource, protocol, year, date)

rm(otterDat_pa, otterDat_po, otterDat_BFC)

## 4. Plot

PO.summ <- otterDat %>%
  filter(protocol == "PO") %>%
  group_by(protocol, gridCell, year) %>%
  summarize(n.repl = n()) %>%
  left_join(grid[, c("geometry", "gridCell")], by = "gridCell")%>%
  st_as_sf 
  
replicates.summ <- otterDat %>%
  filter(protocol != "PO") %>%
  group_by(protocol, gridCell, year) %>%
  summarize(n.repl = n()) %>%
  left_join(grid[, c("geometry", "gridCell")], by = "gridCell")%>%
  st_as_sf 

ggplot(replicates.summ) +
  stat_ecdf(aes(x = n.repl, color = protocol), size = 2, show.legend = F)+
  scale_x_continuous(breaks = 1:10, limits = c(1,10))+
  facet_wrap(~protocol)+
  theme_bw()

ggplot(replicates.summ)+
  geom_sf(data = map_FR) +
  geom_sf(aes(fill = n.repl)) + 
  scale_fill_gradient2(
    low = "white",
    high = "red"
  ) + 
  facet_wrap(~year)+
  theme_bw()

### SAVE -----------------------------------------------------------------------

saveRDS(otterDat, "data/otterDat.rds")

# otterDat <- readRDS("data/otterDat.rds")
 
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

otterDat <- otterDat %>%
  mutate(period = year %/% 4)

otterDat %>%
  filter(protocol != "PO") %>% 
  st_drop_geometry() %>%
  group_by(period, gridCell) %>%
  summarize(presence = any(as.logical(presence)),
            nsample = n()) %>%
  left_join(grid[, c("geometry", "gridCell")], by = "gridCell") %>%
  st_as_sf %>%
  ggplot() +
  geom_sf(data = map_FR) +
  geom_sf(aes(fill = presence)) +
  geom_sf(data = otterDat %>% 
            filter(protocol == "PO") %>% 
            st_as_sf(coords = c("lon", "lat"),crs = 2154), aes(color = protocol), size = .25) +
  scale_fill_manual(name = "Standardized data",
                    values = c("orange", "darkblue"),
                    labels = c("Unobserved", "present")) +
  scale_color_manual("Opportunistic data", values = "black", label = "present") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5),
         fill = guide_legend(title.position="top", title.hjust = 0.5))+
  facet_wrap( ~ paste0(period * 4, " - ", period * 4 + 3))
  # facet_wrap( ~ year)

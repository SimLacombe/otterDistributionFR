library(tidyverse)
library(foreach)
library(sf)
library(raster)

rm(list = ls())

dat.filename <- "data/otterDat.rds"

map_FR <- readRDS("data/map_fr.rds") %>%
  st_as_sf(crs = 2154)

grid <- readRDS("data/L9310x10grid.rds") %>%
  st_as_sf(crs = 2154)

dat <- readRDS(dat.filename)

datPO<- dat %>% 
  filter(protocol == "PO")

# subset of data provided by major contributers
datMC <- datPO %>%
  group_by(observer) %>%
  filter(n()>=5)

### entropy --------------------------------------------------------------------

# Hdf <- datPO %>%
#   group_by(region, year, observer) %>%
#   summarize(fObs = n()) %>%
#   group_by(region, year) %>%
#   mutate(f = fObs/sum(fObs)) %>%
#   summarize(H = - sum(f * log(f, base = 2)))
# 
# Hdf <- left_join(Hdf, map_FR, by = "region", all.) %>%
#   st_as_sf()
# 
# ggplot(Hdf)+
#   geom_sf(aes(fill = H))+
#   geom_point(data = dat %>% filter(protocol == "PO"), aes(x = lon, y = lat), size = .5)+
#   facet_wrap(~year)

### Cumulated sampled surface --------------------------------------------------

getSamplingArea <- function(x, y, res = 100, bw = 25000, offset = st_bbox(map_FR)[c(1,3,2,4)] + c(-1,1,-1,1)*25000, lvl){
  kde <- MASS::kde2d(x, y, n = res, h = c(bw, bw), lims = offset)
  
  contour_values <- sort(kde$z)
  contour_level <- contour_values[which.max(cumsum(contour_values) / sum(contour_values) >= lvl)]
  contour_lines <- contourLines(x = kde$x, y = kde$y, z = kde$z, levels = contour_level)
  
  contour_sf <- do.call(st_sfc, lapply(contour_lines, function(contour) {
    coords <- cbind(contour$x, contour$y)
    st_polygon(list(coords))
  })) %>% st_union()
  
  contour_sf
}

activeAreas <- datMC %>%
  group_by(observer) %>% 
  summarize(activeArea = getSamplingArea(lon, lat, lvl = 0.05))%>%
  mutate(activeSurface = st_area(activeArea)) 

activeAreas <-  datMC %>%
  group_by(year, observer) %>%
  summarize() %>%
  left_join(activeAreas, by = "observer") %>%
  st_as_sf(crs = 2154)

effort_intraRegion <- foreach(yr = 2009:2023, .combine = rbind) %do%{

  eff <- activeAreas %>%
    filter(year == yr) %>%
    st_intersects(grid, .)
  
  grid %>% 
  mutate(year = yr,
         eff = sapply(eff, length))
}

ggplot(effort_intraRegion) +
  geom_sf(aes(fill = eff), col = NA) +  
  # geom_sf(data = st_as_sf(datPO, coords= c("lon", "lat"), crs = 2154), size = 0.5)+
  scale_fill_gradient2(
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint=8
  ) + 
  facet_wrap(~year)+
  theme_bw()

### get buffer for each structure ----------------------------------------------

dataRegions <- datPO %>%
  group_by(dataSource) %>% 
  summarize(activeArea = getSamplingArea(lon, lat, lvl = 0.001))%>%
  mutate(activeSurface = st_area(activeArea))

effort_interRegion <- datPO %>% 
  group_by(year, dataSource) %>%
  summarize() %>% 
  left_join(dataRegions, by = "dataSource") %>%
  st_as_sf(crs = 2154)

ggplot(effort_interRegion) + 
  geom_sf(data = map_FR) + 
  geom_sf(aes(fill = dataSource), alpha = .75) + 
  geom_sf(data = datPO %>% st_as_sf(coords = c("lon", "lat"), crs = 2154), size = .5) +
  facet_wrap(~year)

library(tidyverse)
library(foreach)
library(sf)
library(raster)

rm(list = ls())

dat.filename <- "data/otterDat.rds"

map_FR <- readRDS("data/map_fr.rds") %>%
  st_as_sf(crs = 2154)

dat <- readRDS(dat.filename)

datPO<- dat %>% 
  filter(protocol == "PO")

### entropy --------------------------------------------------------------------

Hdf <- datPO %>%
  group_by(region, year, observer) %>%
  summarize(fObs = n()) %>%
  group_by(region, year) %>%
  mutate(f = fObs/sum(fObs)) %>%
  summarize(H = - sum(f * log(f, base = 2)))

Hdf <- left_join(Hdf, map_FR, by = "region", all.) %>%
  st_as_sf()

### Cumulated sampled surface --------------------------------------------------

getSamplingArea <- function(x, y, res = 100, bw = 25000, offset = st_bbox(map_FR)[c(1,3,2,4)] + c(-1,1,-1,1)*25000, lvl){
  kde <- kde2d(x, y, n = res, h = c(bw, bw), lims = offset)
  
  contour_values <- sort(kde$z)
  contour_level <- contour_values[which.max(cumsum(contour_values) / sum(contour_values) >= lvl)]
  contour_lines <- contourLines(x = kde$x, y = kde$y, z = kde$z, levels = contour_level)
  
  contour_sf <- do.call(st_sfc, lapply(contour_lines, function(contour) {
    coords <- cbind(contour$x, contour$y)
    st_polygon(list(coords))
  })) %>% st_union()
  
  contour_sf
}

activeAreas <- datPO %>%
  group_by(observer) %>%
  filter(n()>=5 & observer != "unknown") %>%
  group_by(observer) %>% 
  summarize(n = n(),
            activeArea = getSamplingArea(lon, lat, lvl = 0.05))

activeAreas <- activeAreas %>%
  mutate(activeSurface = st_area(activeArea)) %>%
  arrange(activeSurface)

samplingEffort <- datPO %>%
  group_by(observer) %>%
  filter(n()>=5 & observer != "unknown") %>%
  group_by(observer, year) %>% 
  summarize() %>%
  left_join(activeAreas, by = "observer") %>%
  st_as_sf(crs = 2154) 


ggplot(samplingEffort %>% st_as_sf(crs = 2154)) +
  geom_sf(aes(fill = as.numeric(activeSurface))) + 
  geom_point(data = dat %>% filter(protocol == "PO", !region %in% c("PoCha", "Au", "RA")), aes(x = lon, y = lat), size = .5)+
  facet_wrap(~year) + 
  theme_bw()

ggplot(Hdf)+
  geom_sf(aes(fill = H))+
  geom_point(data = dat %>% filter(protocol == "PO"), aes(x = lon, y = lat), size = .5)+
  facet_wrap(~year)

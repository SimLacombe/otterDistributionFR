library(tidyverse)
library(sf)

source("src/functions/getReplicates.R")

rm(list = ls())

### Load data ------------------------------------------------------------------

data.filename <- "data/otterDat.rds"
dataRaw.filename <- "data/otterDatRaw.rds"
grid.filename <- "data/landscape.rds"
effort.filename <- "data/samplingEffort.rds"

otterDat <- readRDS(data.filename)
otterDatRaw <- readRDS(dataRaw.filename)
effort <- readRDS(effort.filename)
landscape <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

### Plot full dataset ----------------------------------------------------------

stdSummary <- otterDat %>%
  filter(protocol != "PO") %>% 
  group_by(gridCell) %>%
  summarize(presence = sign(sum(presence))) %>% 
  left_join(landscape %>% select(gridCell)) %>%
  mutate(dataType = "Standardized") %>%
  st_as_sf(crs = 2154)

oppSummary <- otterDat %>%
  filter(protocol == "PO") %>% 
  group_by(gridCell) %>%
  summarize(presence = sum(presence)) %>% 
  left_join(landscape %>% select(gridCell)) %>%
  mutate(dataType = "Opportunistic") %>%
  st_as_sf(crs = 2154)

dataSummary <- rbind(stdSummary, oppSummary)

ggplot(landscape) +
  geom_sf(col = NA) + 
  geom_sf(data = dataSummary %>% filter(dataType == "Opportunistic"),
          aes(fill = presence), col = NA) +
  scale_fill_gradient(name = "", trans = "log", breaks = c(2.5,10,40,160)) +
  ggnewscale::new_scale_fill()+
  geom_sf(data = dataSummary %>% filter(dataType == "Standardized"),
          aes(fill = factor(presence)))+
  scale_fill_manual(name = "", labels = c("absent", "present"), values = c("orange", "chartreuse4"))+
  facet_wrap(~dataType) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

### Plot action areas ----------------------------------------------------------

regions <- c("83", "91", "73", "93", "82")

landscape2 <- landscape %>%
  filter(code_insee %in% regions)

otterDatRaw2 <- otterDatRaw %>%
  filter(protocol == "PO") %>%
  filter(dataSource %in% c("LPO-Occitanie", "LPO-PACA", "LPO-AuRA"))  

actionAreas <- otterDatRaw2 %>%
  group_by(dataSource) %>% 
  summarize(aa = getSamplingArea(lon, lat, lvl = 0.001, 
                                 offset = st_bbox(landscape2)[c(1,3,2,4)] + c(-1,1,-1,1)*25000)) %>%
  st_as_sf(crs = 2154)

otterDatRaw2 <- otterDatRaw2 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2154)

plot1 <- ggplot(otterDatRaw2)+
  geom_sf(data = st_union(landscape2))+
  geom_sf(data = actionAreas, aes(fill = dataSource), alpha = 0.33)+
  geom_sf(aes(col = dataSource), size = 0.25)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

years <- c(2009, 2019)

effort2 <- effort[landscape$code_insee %in% regions, ]
colnames(effort2) <- paste0("yr.", 2009:2023)

landscape2 <- cbind(landscape2, effort2[, years - 2008]) %>%
  pivot_longer(cols = all_of(paste0("yr.", years)), names_to = "year", values_to = "dataSource") %>%
  mutate(year = as.numeric(gsub("\\D", "", year)))

plot2 <- ggplot(landscape2 %>%
                  filter(dataSource %in% c("LPO-Occitanie", "LPO-PACA", "LPO-AuRA")))+
  
  geom_sf(data = st_union(landscape2))+
  geom_sf(aes(fill = dataSource), col = NA)+
  geom_sf(data = otterDatRaw2 %>% filter(year %in% years, presence == 1), size = 0.25)+
  facet_wrap(~year, ncol = 1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggpubr::ggarrange(plot1, plot2, common.legend = TRUE, legend = "bottom")

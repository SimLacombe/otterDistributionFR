library(tidyverse)
library(sf)

rm(list = ls())

### Load data ------------------------------------------------------------------

data.filename <- "data/otterDat.rds"
grid.filename <- "data/L9310x10grid.rds"
effort.filename <- "data/samplingEffort.rds"

otterDat <- readRDS(data.filename)

L93_grid <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

### Plot full dataset ----------------------------------------------------------

stdSummary <- otterDat %>%
  filter(protocol != "PO") %>% 
  group_by(gridCell) %>%
  summarize(presence = sign(sum(presence))) %>% 
  left_join(L93_grid %>% select(gridCell)) %>%
  mutate(dataType = "Standardized") %>%
  st_as_sf(crs = 2154)

oppSummary <- otterDat %>%
  filter(protocol == "PO") %>% 
  group_by(gridCell) %>%
  summarize(presence = sum(presence)) %>% 
  left_join(L93_grid %>% select(gridCell)) %>%
  mutate(dataType = "Opportunistic") %>%
  st_as_sf(crs = 2154)

dataSummary <- rbind(stdSummary, oppSummary)


ggplot(L93_grid) +
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

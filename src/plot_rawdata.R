library(tidyverse)
library(sf)
library(foreach)
library(mgcv)

library(rjags)
library(coda)

rm(list = ls())

source("src/functions/jags_ini.R")

# Crop Paris + NE
REGIONS <- c("72", "83", "25", "26", "53",
             "24", "43", "23", "91",
             "74", "73", "52",
             "54", "93", "82")

TIMEPERIOD <- 1 #years

TIMELAG <- TIMEPERIOD - 2009 %% TIMEPERIOD

### Load data ------------------------------------------------------------------

data.filename <- "data/otterDat.rds"
grid.filename <- "data/L9310x10grid.rds"
effort.filename <- "data/samplingEffort.rds"

otterDat <- readRDS(data.filename)

L93_grid <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

effort <- readRDS(effort.filename)

### Plot full dataset ----------------------------------------------------------

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
  # facet_wrap( ~ paste0(period * 4, " - ", period * 4 + 3))
  facet_wrap( ~ year)

### Plot sampled pixels  -------------------------------------------------------

periods <- (2009:2023 + TIMELAG) %/% TIMEPERIOD

effort <- effort[L93_grid$code_insee %in% REGIONS, ]

if(TIMEPERIOD > 1) {
  effort <- sapply(unique(periods), function(p) {
    sign(apply(effort[, periods == p], 1, sum))
  })
}

pixels <- cbind(L93_grid, effort) %>%
  pivot_longer(cols = all_of(paste0("X", 1:15)),
               names_to = "year", values_to = "effort") %>%
  mutate(year = as.numeric(gsub("X", "", year))+2008)

pixels <- otterDat %>% 
  group_by(year, gridCell) %>%
  filter(any(protocol != "PO")) %>%
  summarize(is.sampled = 1 )%>%
  left_join(pixels, ., by = c("year", "gridCell")) 

pixels <- pixels %>%
  mutate(is.sampled = ifelse(is.na(is.sampled), 0, is.sampled)) %>%
  mutate(is.sampled = effort | is.sampled)

ggplot(pixels) + 
  geom_sf(aes(fill = is.sampled), col = NA) + 
  facet_wrap(~year)

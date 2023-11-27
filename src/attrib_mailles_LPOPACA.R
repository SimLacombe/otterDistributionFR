library(tidyverse)
library(foreach)
library(sf)
library(concom)

rm(list = ls())

source("src/utility_functions.R")

### Load data ###

data.filename <- "data/otterDat.rds"

otterDat <- readRDS(data.filename) %>% 
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)

otterDat <- otterDat %>% 
  filter(region == "PACA")


map.filename <- "data/map_fr.rds"
grid.filename <- "data/L9310x10grid.rds"

map <- readRDS(map.filename) %>%
  filter(code_insee == "93")

L93_grid <- readRDS(grid.filename) %>%
  st_intersection(map) %>%
  dplyr::select(grid.cell)

mailles_pna <- read_sf("data/PNA-data/PACA-LPOPACA/attributions_maille_pna/") %>%
  st_transform(crs = 2154)

mailles_pna <- mailles_pna %>% 
  rename(ATR_2019 = attribuée,
         grid.cell = CODE10KM) %>%
  select(ATR_2019, ATR_2020, ATR_2021, ATR_2022)

mailles_pna <- mailles_pna %>%
  mutate(ATR_2019 = ifelse(ATR_2019 == "oui", 1, NA),
         ATR_2020 = ifelse(ATR_2020 == "oui", 1, NA),
         ATR_2021 = ifelse(ATR_2021 == "oui", 1, NA))

mailles_pna <- mailles_pna %>%
  pivot_longer(cols = paste("ATR", 2019:2022, sep = "_"), names_to = "year", values_to = "surveyed") %>% 
  mutate(year = as.numeric(substr(year, 5,8)))

mailles_pna$surveyed[is.na(mailles_pna$surveyed)] <- 0

mailles_pna$surveyed <- as.logical(mailles_pna$surveyed)

ggplot(map)+
  geom_sf()+
  geom_sf(data = mailles_pna %>% filter(surveyed), col = "lightblue", alpha = 0.5, aes(fill = surveyed)) +
  geom_sf(data = otterDat, aes(color = factor(presence), pch = PNA.protocole))+
  scale_fill_manual(values = "lightblue", name = "", labels = "Carré attribué")+
  scale_color_manual(values = c("orange", "chartreuse4"), name = "", labels = c("absent", "présent"))+
  scale_shape_manual(values = c(4,19), name = "", labels = c("opportuniste", "protocolé"))+
  facet_wrap(~year)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))


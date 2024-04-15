library(tidyverse)
library(lubridate)
library(sf)

rm(list = ls())

dat <- readRDS("data/otterDat.rds") %>%
  st_as_sf(crs = 2154)
map_FR <- readRDS("data/map_fr.rds")%>%
  st_as_sf(crs = 2154)

ggplot(dat%>% filter(PA))+
  geom_sf(data = map_FR) + 
  geom_sf(aes(col = PA.protocole))+
  # facet_wrap(~ paste0(year%/%2 * 2, " - ", year%/%2 * 2 +1 )) +
  theme_bw() 
  
ggplot(dat%>% filter(data_region == "PdL")%>% 
         mutate(PA.protocole = ifelse(is.na(PA.protocole), "opp", PA.protocole)))+
  geom_sf(data = map_FR %>% filter(data_region %in% c("PdL"))) + 
  geom_sf(aes(col = PA.protocole))+
  facet_wrap(~ paste0(year%/%2 * 2, " - ", year%/%2 * 2 +1 )) +
  theme_bw() 

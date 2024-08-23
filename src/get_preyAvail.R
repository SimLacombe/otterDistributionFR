library(tidyverse)
library(purrr)
library(sf)

rm(list = ls())

landscape.filename <- "data/landscape.rds"
preyData.filename <- "data/rawPreyData.rds"

### Get Data -------------------------------------------------------------------

landscape <- read_rds(landscape.filename) %>%
  st_as_sf(crs = 2154)

preyData <- read_rds(preyData.filename) %>%
  st_as_sf(crs = 2154)

### Summarize presence of the two taxas ----------------------------------------
# TAC Oncorhynchus mykiss - Truite arc-en-ciel
# TRF Salmo trutta fario - Truite commune 
# TRM Salmo trutta trutta - Truite de mer

# APP Austropotamobius pallipes écrevisse à pieds blancs
# OCL Orconectes limosus écrevisse américaine
# PFL Pacifastacus leniusculus écrevisse de Californie
# PCC Procambarus clarkii  écrevisse de Louisiane

preyData <- preyData %>%
  mutate(trout = sign(sum(TAC, TRF, TRM)),
         crayfish = sign(sum(APP, OCL, PFL, PCC)))

### Display on the grid landscape ----------------------------------------------

preyData_gridded <- preyData %>%
  st_intersection(landscape) %>%
  st_drop_geometry() %>% 
  group_by(year, gridCell) %>%
  summarize(Crayfish = sign(sum(crayfish)),
            Trout = sign(sum(trout))) %>%
  left_join(landscape %>% select(gridCell)) %>%
  st_as_sf(crs = 2154)

### Interpolate using inverse distance weighting -------------------------------

allyears <- unique(preyData$year)

preyData_interp <- map(allyears, function(t){
  
  tmp <- preyData_gridded %>%
    filter(year == t)
  
  cells_to_interpolate <- landscape %>%
    filter(!gridCell %in% tmp$gridCell) 
  
  interpolated_cf <-  gstat::idw(Crayfish~1, st_centroid(tmp) , st_centroid(cells_to_interpolate), nmax = 20)  %>%
    st_drop_geometry()
  interpolated_tr <-  gstat::idw(Trout~1, st_centroid(tmp) , st_centroid(cells_to_interpolate), nmax = 20)  %>%
    st_drop_geometry()
  
  interpolated <- cells_to_interpolate %>%
    st_drop_geometry() %>%
    mutate(year = t,
           Crayfish = interpolated_cf$var1.pred,
           Trout = interpolated_tr$var1.pred) %>%
    select(year, gridCell, Crayfish, Trout)
  
  rbind(st_drop_geometry(tmp), interpolated) %>%
    arrange(gridCell)
})

preyData_interp <- reduce(preyData_interp, rbind) %>% 
  left_join(landscape %>% select(gridCell)) %>%
  st_as_sf()

# data is poor in 2023: duplicate the year 2022 

preyData_interp$Crayfish[preyData_interp$year == 2023] <- preyData_interp$Crayfish[preyData_interp$year == 2022]
preyData_interp$Trout[preyData_interp$year == 2023] <- preyData_interp$Trout[preyData_interp$year == 2022]

ggplot(preyData_interp)+
  geom_sf(aes(fill = Crayfish), col = NA) +
  scale_fill_gradient(high = "#9F3000", low = "#2B0000") +
  facet_wrap(~ year) +
  theme_bw()

ggplot(preyData_interp)+
  geom_sf(aes(fill = Trout), col = NA) +
  scale_fill_gradient(high = "#FFD700", low = "#2B2B00") +
  facet_wrap(~ year) +
  theme_bw()

saveRDS(preyData_interp, "data/preyData.rds")

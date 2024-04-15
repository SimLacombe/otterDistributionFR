library(tidyverse)
library(lubridate)
library(sf)

dat.filename <- "data/PNA-data/CentreValdeLoire-Caudalis/"

dat <- read_sf(dat.filename)

dat[, c("lon.l93", "lat.l93")] <- st_coordinates(dat)
dat <- st_drop_geometry(dat)

dat <- dat %>%
  mutate(date = as.Date(OBS_DEBUT),
         year = year(date),
         presence = as.numeric(STAT_OBS == "Pr"),
         data.provider = "Caudalis", 
         PA = TRUE,
         PA.protocole  = "point",
         collision = FALSE,
         CT.period = NA)

dat <- dat %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>% 
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

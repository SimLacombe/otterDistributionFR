library(tidyverse)
library(lubridate)
library(sf)

dat1.filename <- "data/PNA-data/Aquitaine-GREGE/Grege_Données LLU tot_mai2021.csv"
etudes_transect <- c("Euskadour", "BPL", "A63 Ondres", "LGV SEA", "A65 Suivi", "A65 2014",
                     "LGV SEA 2013", "LGV BET", "LGV BET/Euskadour")

dat1 <- read.csv(dat1.filename, sep = ";", dec = ",") %>%
  filter(PRESENCE_LLU %in% c("Positif", "Négatif"),
         !((Date == "")&(DATE_ == "")))

dat1$date <- dat1$Date
dat1[(dat1$date == ""), "date"] <- dat1[(dat1$date == ""), "DATE_"]

dat1 <- dat1 %>% 
  mutate(date = as.Date(date),
         PA = TRUE,
         PA.protocole = ifelse(ETUDE %in% etudes_transect, "transect", "point"),
         collision = FALSE,
         year = year(date),
         presence = as.numeric(PRESENCE_LLU  == "Positif"),
         data.provider = "GREGE",
         CT.period = NA) %>% 
  rename(lon.l93 = POINT_X,
         lat.l93 = POINT_Y)

dat1 <- dat1 %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat1 <- dat1 %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period) 

dat2.filename <- "data/PNA-data/Aquitaine-GREGE/Fortuites.csv"
dat2 <- read.csv(dat2.filename, sep = ";", dec = ",") %>%
  mutate(date = as.Date(DATE_),
         year = year(date),
         PA = FALSE,
         PA.protocole = NA,
         collision = FALSE,
         presence = 1,
         data.provider = "GREGE",
         CT.period = NA) %>% 
  rename(lon.l93 = POINT_X,
         lat.l93 = POINT_Y) %>%
  filter(year %in% 2009:2023) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period) 


dat3.filename <- "data/PNA-data/Aquitaine-GREGE/Prospections_all.csv"
dat3 <- read.csv(dat3.filename, sep = ";", dec = ",") %>%
  mutate(date = as.Date(DATE_),
         year = year(date),
         PA = TRUE,
         PA.protocole = "point",
         collision = FALSE,
         presence = as.numeric(PRESENCE_LLU == "Positif"),
         data.provider = "GREGE",
         CT.period = NA) %>% 
  rename(lon.l93 = POINT_X,
         lat.l93 = POINT_Y) %>%
  filter(year %in% 2009:2023) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period) 

otterCTObs.filename <- "data/PNA-data/Aquitaine-GREGE/CT_otter.csv"
CTdat.filename <- "data/PNA-data/Aquitaine-GREGE/CT_all.csv"

otterCTObs <- read.csv(otterCTObs.filename, sep = ";", dec = ",") %>%
  group_by(OBJECTID, Date_Relevé) %>%
  summarize(nobs = n())

CT_dat <- read.csv(CTdat.filename, sep = ";", dec = ",") %>% 
  select(OBJECTID, Date_Pose, Date_Relevé, POINT_X, POINT_Y) %>%
  group_by(OBJECTID) %>%
  mutate(Date_Prev = lag(Date_Relevé, 1),
         Date_Prev = ifelse(is.na(Date_Prev), Date_Pose, Date_Prev)) %>%
  ungroup %>%
  left_join(otterCTObs, by = c("OBJECTID", "Date_Relevé")) %>%
  mutate(date = as.Date(Date_Relevé),
         year = year(date),
         PA = TRUE,
         PA.protocole = "CT",
         collision = FALSE,
         presence = ifelse(is.na(nobs), 0, sign(nobs)),
         data.provider = "GREGE",
         CT.period = as.numeric(difftime(Date_Relevé, Date_Prev, units = "day"))) %>% 
  rename(lon.l93 = POINT_X,
         lat.l93 = POINT_Y) %>%
  filter(year %in% 2009:2023) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))%>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period) 

dat <- rbind(dat1, dat2, dat3, CT_dat)
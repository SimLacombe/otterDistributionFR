require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/Aquitaine-GREGE/Grege_Données LLU tot_mai2021.xlsx"

dat <- readxl::read_xlsx(dat.filename) %>%
  filter(PRESENCE_LLU %in% c("Positif", "Négatif"),
         !(is.na(Date)&is.na(DATE_)))

dat$date <- dat$Date
dat[is.na(dat$date), "date"] <- dat[is.na(dat$date), "DATE_"]

dat <- dat %>% 
  mutate(PNA.protocole = grepl("LGV", ETUDE)|grepl("Euskadour", ETUDE)|grepl("BPL", ETUDE),
         year = year(date),
         presence = as.numeric(PRESENCE_LLU  == "Positif"),
         region = "Aquitaine", 
         data.provider = "GREGE",
         loc = NA) %>% 
  rename(lon.l93 = POINT_X,
         lat.l93 = POINT_Y)

dat <- dat %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat <- dat %>%
  select(data.provider, region,PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence) 


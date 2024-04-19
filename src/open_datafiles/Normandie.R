library(tidyverse)
library(lubridate)

dat1.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN_20210518.xlsx"
dat2.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN_20231010.xlsx"

protocoles <- c("COLL", "CT", "UICN", "PP")

dat1 <- readxl::read_xlsx(dat1.filename) %>%
  select(-`N° de parcelle`)

dat2 <- readxl::read_xlsx(dat2.filename) %>%
  rename(ORGANISME = `Nom Observateur`)

dat <- rbind(dat1,dat2) 

dat <- dat %>% 
  mutate(date = as.Date(`Date\r\nobservation`),
         year = year(date),
         presence = as.numeric(`Comportement\r\nN°1` != "0001/Absence d'indice"),
         data.provider = "GMN") %>%
  rename(lon.l93 = `Longitude Lambert 93`,
         lat.l93 = `Latitude Lambert 93`) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))
dat <- dat %>% 
  mutate(comm = paste0(toupper(`Commentaire\r\ninventaire`), " ", toupper(`Commentaire\r\ncitation`)),
         COLL = `Comportement\r\nN°1` == "2315/Mort par collision avec véhicule",
         CT = grepl("PIÈGE", comm),
         UICN = grepl("UICN", comm)|grepl("\\d\\d\\dM", comm)|grepl("\\d\\d\\d M", comm)|grepl("KM", comm),
         PP = TRUE)

dat <- dat %>% 
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  group_by(ID, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = protocole %in% c("UICN", "PP"),
         PA.protocole = ifelse(protocole == "UICN", "transect", ifelse(protocole == "PP", "point", NA)),
         collision = protocole == "COLL",
         CT.period = NA) %>%
  ungroup %>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

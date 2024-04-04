library(tidyverse)
library(lubridate)

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"

protocoles <- c("COLL", "MB", "LL", "ASF", "RNNBA", "VACH", "CD", "OPP")
  
dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT),
         TRA_NAME = toupper(TRA_NAME),
         TRA_SURNAME = toupper(TRA_SURNAME),
         COLL = (HAS_DEATH_INFO == "Oui"&grepl("ROUT", COMMENT))|grepl("COLLISION", COMMENT)|grepl("ÉCRASÉ", COMMENT)|grepl("PERCUTÉ", COMMENT),
         MB = (NAME == "MARAIS BRETON"|TRA_NAME == "MARAIS BRETON"),
         LL = grepl("LOUTRE LITTORAL", COMMENT),
         ASF = grepl("ASF", COMMENT),
         RNNBA = grepl("RNNBA", COMMENT),
         VACH = grepl("RNRVACHERIE", TRA_NAME)|grepl("RNRVACHERIE", TRA_SURNAME),
         CD = grepl("DUPÉ", TRA_NAME),
         OPP = !COLL&!MB&!LL&!ASF&!RNNBA&!VACH&!CD)

dat <- dat %>%
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  mutate(date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         data.provider = "LPO-PdL") %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME) %>%
  group_by(ID_SIGHTING, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = (protocole != "OPP"),
         PA.protocole = ifelse(protocole %in% c("MB", "LL", "CD"), "transect", ifelse(!protocole %in% c("OPP", "CT", "COLL"), "point", NA)),
         collision = protocole == "COLL",
         CT.period = NA) %>%
  ungroup %>% 
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

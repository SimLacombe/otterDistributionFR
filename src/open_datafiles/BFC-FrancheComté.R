require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/BourgogneFrancheComté-LPOBFA/Copie de Données_loutre_LPOBFC_thèse.csv"
dat <- read.csv(dat.filename, sep = ";")

dat$Remarque <- iconv(dat$Remarque, from = "ISO-8859-1", to = "UTF-8")%>%toupper

dat <- dat %>% 
  mutate(PNA.protocole = (Protocole == "Protocole standard maille 10x10km adapt\xe9 \xe0 la Franche-Comt\xe9"|grepl("PRA", Remarque)|grepl("PAR", Remarque)), 
         date = as.Date(Date, format = "%d/%m/%Y"),
         loc = NA,
         presence = sign(Nombre),
         data.provider = "LPO",
         grid.cell = Maille,
         year = year(date),
         lon.l93 = NA,
         lat.l93 = NA) %>%
  select(data.provider, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

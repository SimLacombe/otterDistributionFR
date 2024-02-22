require(tidyverse, lubridate)

dat1.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/données_loutre_SHNA_brut.csv"
dat2.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/BBF20231005_Loutre_etudeCNRS_SFEPM_2021_2023.csv"

dat1 <- read.csv(dat1.filename, sep = ";") %>%
  filter(LAMBERT_93 != "") %>%
  mutate(PROTOCOLE = "SFEPM-Loutre",
         VIVANT = "FAUX") #for now, I need to find a solution to this issue
dat2 <- read.csv(dat2.filename, sep = ";")

dat <- rbind(dat1[, names(dat1) %in% names(dat2)], dat2[, names(dat2) %in% names(dat1)]) %>% 
  filter(DATE_OBS != "") %>%
  mutate(PNA.protocole = PROTOCOLE == "SFEPM-Loutre", 
         date = as.Date(DATE_OBS),
         loc = COMMUNE,
         presence = as.numeric(VIVANT=="VRAI"|EMPREINTES=="VRAI"|CROTTES=="VRAI"),
         data.provider = "SHNA",
         grid.cell = LAMBERT_93,
         year = year(date),
         lon.l93 = NA,
         lat.l93 = NA) %>%
  select(data.provider, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)


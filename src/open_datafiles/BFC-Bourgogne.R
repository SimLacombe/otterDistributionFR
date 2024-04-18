require(tidyverse, lubridate)

dat1.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/données_loutre_SHNA_brut.csv"
dat2.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/BBF20231005_Loutre_etudeCNRS_SFEPM_2021_2023.csv"

dat1 <- read.csv(dat1.filename, sep = ";") %>%
  filter(LAMBERT_93 != "") %>%
  mutate(TYPE = toupper(TYPE),
         PA = (TYPE != "")|grepl("maille", COMMENTAIRE_RELEVE)|grepl("\\d\\d\\d m", COMMENTAIRE_RELEVE)|grepl("\\d\\d\\dm", COMMENTAIRE_RELEVE), 
         PA.protocole = ifelse(PA&grepl("PONT", TYPE), "point", ifelse(PA, "transect", NA)),
         collision = FALSE,
         date = as.Date(DATE_OBS),
         presence = as.numeric(EMPREINTES=="VRAI"|CROTTES=="VRAI"),
         data.provider = "SHNA - OFAB",
         grid.cell = LAMBERT_93,
         year = year(date),
         lon.l93 = NA,
         lat.l93 = NA,
         CT.period = NA) %>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)


dat2 <- read.csv(dat2.filename, sep = ";") %>% 
  filter(DATE_OBS != "",
         METHODE != "Piège photo") %>%
  mutate(TYPE = toupper(TYPE),
         PA.protocole = ifelse(PROTOCOLE == "SFEPM-Loutre", "transect", 
                               ifelse(grepl("PONT", TYPE)|grepl("BUSE", TYPE)|grepl("VIADUC", TYPE), "point", NA)),
         PA = PA.protocole %in% c("transect", "point"),
         collision = FALSE,
         date = as.Date(DATE_OBS),
         presence = as.numeric(VIVANT=="VRAI"|EMPREINTES=="VRAI"|CROTTES=="VRAI"),
         data.provider = "SHNA",
         grid.cell = LAMBERT_93,
         year = year(date),
         lon.l93 = NA,
         lat.l93 = NA,
         CT.period = NA) %>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

dat <- rbind(dat1, dat2)
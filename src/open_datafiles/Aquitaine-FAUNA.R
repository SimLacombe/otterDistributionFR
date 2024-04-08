library(lubridate)
library(tidyverse)
library(sf)

data.path <- "data/PNA-data/Aquitaine-SINP/Point.csv"
metadata.path <- "data/PNA-data/Aquitaine-SINP/Metadonnees.csv"

gestionnaires <- c("CEN Nouvelle-Aquitaine", "INPN", "CD 40", "Landes Nature",
                   "CD 33","PNR Landes de Gascogne", "CDC Biodiversité", 
                   "BIOTOPE", "Vienne Nature", "Cistude Nature")

dat <- read.csv(data.path, sep = "\t")

metadat <- read.csv(metadata.path, sep = "\t") %>%
  rename(IdJdd = IdJeuDonnees)

dat <- dat %>%
  left_join(metadat[, c("IdJdd", "NomJeuDonnees", "GestionnaireJdd")]) %>%
  mutate(NomJeuDonnees = tolower(NomJeuDonnees)) %>%
  filter(grepl("opportuniste", NomJeuDonnees)|grepl("naturaliste", NomJeuDonnees)|grepl("faunistique", NomJeuDonnees),
         !GestionnaireJdd %in%c("GREGE", "GRIFS", "GMHL"),
         !grepl("RUYS", GestionnaireJdd),
         !StatPresen == "absent")
   
dat <- dat %>%
  mutate(date = as.Date(DateDebut),
         year = year(date),
         PA = FALSE,
         PA.protocole = NA,
         collision = FALSE,
         presence = 1,
         data.provider = GestionnaireJdd,
         CT.period = NA)

dat <- dat %>%
  separate(GeomWkt, into = c("srid", "geometry"), sep = ";") %>%
  mutate(
    geometry = str_remove(geometry, "POINT\\("),
    geometry = str_remove(geometry, "\\)")
  ) %>%
  separate(geometry, into = c("x", "y"), sep = " ") %>%
  mutate(
    lon.l93 = as.numeric(x),
    lat.l93 = as.numeric(y)
  )

dat <- dat %>% 
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period) 


library(lubridate)
library(tidyverse)
library(sf)

data1.path <- "data/PNA-data/Aquitaine-SINP/Point.csv"
data2.path <- "data/PNA-data/Aquitaine-SINP/Polygon.csv"
metadata.path <- "data/PNA-data/Aquitaine-SINP/Metadonnees.csv"

metadat <- read.csv(metadata.path, sep = "\t") %>%
  rename(IdJdd = IdJeuDonnees)

dat1 <- read.csv(data1.path, sep = "\t") %>%
  st_as_sf(wkt = "GeomWkt")%>%
  left_join(metadat[, c("IdJdd", "NomJeuDonnees", "GestionnaireJdd")]) %>%
  mutate(GestionnaireJdd = ifelse(grepl("Indépendant", GestionnaireJdd), "Indépendant", GestionnaireJdd)) %>%
  filter(grepl("opportuniste", NomJeuDonnees)|grepl("naturaliste", NomJeuDonnees)|grepl("faunistique", NomJeuDonnees),
         !GestionnaireJdd %in%c("GREGE", "GRIFS", "GMHL"),
         !grepl("RUYS", GestionnaireJdd),
         !StatPresen == "absent") %>%
  group_by(GestionnaireJdd) %>%
  filter(n()> 100) %>%
  ungroup

dat2 <- read.csv(data2.path, sep = "\t") %>%
  st_as_sf(wkt = "GeomWkt") %>%
  left_join(metadat[, c("IdJdd", "NomJeuDonnees", "GestionnaireJdd")]) %>%
  filter(GestionnaireJdd == "LPO France")

dat1[, c("lon.l93", "lat.l93")] <- st_coordinates(dat1)
dat2[, c("lon.l93", "lat.l93")] <- st_coordinates(st_centroid(dat2))

dat1 <- st_drop_geometry(dat1)
dat2 <- st_drop_geometry(dat2)

dat1 <- dat1 %>%
  mutate(date = as.Date(DateDebut),
         year = year(date),
         PA = FALSE,
         PA.protocole = NA,
         collision = FALSE,
         presence = 1,
         data.provider = GestionnaireJdd,
         CT.period = NA)%>% 
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period) 

dat2 <- dat2 %>%
  rename(grid.cell = Maille10) %>%
  mutate(date = as.Date(DateDebut),
         year = year(date),
         PA = FALSE,
         PA.protocole = NA,
         collision = FALSE,
         presence = 1,
         CT.period = NA) %>%
  separate(NomJeuDonnees, into = c("tmp..", "data.provider", "..tmp"), sep = '\"') %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

dat <- rbind(dat1, dat2)
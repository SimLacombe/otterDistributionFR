library(tidyverse)
library(lubridate)
library(sf)

dat.filename <- "data/PNA-DATA/Bretagne-GMB/Data_Lutra_GMB.csv"
dat <- read.csv(dat.filename, sep = ";")

dat[, c("lon", "lat")] <- dat %>%
  st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"), crs = 4326) %>%
  st_transform(crs = 2154) %>%
  st_coordinates()

dat <- dat %>%
  filter(etat_biologique != "Trouvé mort : impact routier") %>%
  addProtocol(
    patterns = c("Inventaire Mammifères semi-aquatiques de l'Atlas|prospections Loutre standardisées|études Loutre GMB"),
    protocol = IUCN,
    col1 = jdd_nom
  ) %>%
  addProtocol(
    patterns = c("Données opportunistes|Données faunebretagne"),
    protocol = GMBPO,
    col1 = jdd_nom
  )%>%
  arrangeProtocols(IUCN, GMBPO)


dat <- dat %>%
  formatData(dataSourceStr = "GMB",
             protocolCol = protocol,
             dateCol = date_debut,
             presenceCond = statut_observation  == "Présent",
             xCol = lon,
             yCol = lat,
             dateformat = "%d/%m/%Y") 

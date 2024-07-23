library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

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
    protocol = PO,
    col1 = jdd_nom
  )%>%
  arrangeProtocols(IUCN, PO)


dat <- dat %>%
  formatData(dataSourceStr = "GMB",
             observerCol = ifelse(protocol == "PO", ifelse(grepl(" - ", jdd_nom),
                                                           str_split_i(jdd_nom, " - ", 2),
                                                           str_split_i(jdd_nom, " de ", 2)),
                                  NA),
             protocolCol = protocol,
             dateCol = date_debut,
             presenceCond = statut_observation  == "Présent",
             xCol = lon,
             yCol = lat,
             dateformat = "%d/%m/%Y") 

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c())

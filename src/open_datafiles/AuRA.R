library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/RhônesAlpes-LPOAURA&GMA/data_loutre_ORB_AURA.gpkg"

dat.sf <- read_sf(dat.filename)
dat <- st_drop_geometry(dat.sf)
dat[, c("lon", "lat")] <- dat.sf %>%
  st_coordinates()
rm(dat.sf)

dat <- dat %>%
  mutate(comment = paste0(toupper(comment), " ", toupper(comment_priv))) %>%
  filterCamTrap(col = comment) %>%
  filter(is.na(mortalite_cause)|mortalite_cause != "ROAD_VEHICLE") %>%
  addProtocol(
    patterns = c("visionature",
                 "IUCN|UICN|PROTOCOL|PRA LOUTRE"),
    protocol = IUCN,
    col1 = desc_source,
    col2 = comment
  ) %>%
  addProtocol(
    patterns = c("visionature",
                 "LOUTRE-CASTOR|CASTOR-LOUTRE"),
    protocol = AURALC,
    col1 = desc_source,
    col2 = comment
  ) %>%
  addProtocol(
    patterns = c("visionature",
                 "PROSPECTION LOUTRE|RECHERCHE LOUTRE|SUIVI LOUTRE"),
    protocol = AURAPL,
    col1 = desc_source,
    col2 = comment
  ) %>%
  addProtocol(
    patterns = c("visionature",
                 "PONCTUEL"),
    protocol = AURAPP,
    col1 = desc_source,
    col2 = comment
  ) %>%
  addProtocol(
    patterns = c("visionature",
                 "PROSPECTION CIBLÉE"),
    protocol = AURAPCS,
    col1 = desc_source,
    col2 = comment
  ) %>%
  addProtocol(
    patterns = c("visionature"),
    protocol = PO,
    col1 = desc_source
  ) %>%
  arrangeProtocols(IUCN, AURALC, AURAPL, AURAPP, AURAPCS, PO)

dat <- dat %>%
  formatData(dataSourceStr = "LPO-AuRA",
             protocolCol = protocol,
             observerCol = "",
             dateCol = date,
             presenceCond = is_present,
             xCol = lon,
             yCol = lat,
             dateformat = "%Y-%m-%d")

surveys$transect <- append(surveys$transect, c("AURALC", "AURAPL"))
surveys$pointwise <- append(surveys$pointwise, c("AURAPP", "AURAPCS"))
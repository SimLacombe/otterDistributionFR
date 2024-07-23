library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/CentreValdeLoire-Caudalis/"

dat <- read_sf(dat.filename)

dat[, c("lon", "lat")] <- st_coordinates(dat)
dat <- st_drop_geometry(dat)

dat <- dat %>%
  addProtocol(
    patterns = c("SANSAULT|AUBRY"),
    protocol = IUCN,
    col1 = NOM_OBS
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = NOM_OBS
  ) %>%
  arrangeProtocols(IUCN, PO)

dat <- dat %>%
  formatData(dataSourceStr = "Caudalis",
             observerCol = NOM_OBS,
             protocolCol = protocol,
             dateCol = OBS_DEBUT,
             presenceCond = STAT_OBS == "Pr",
             xCol = lon,
             yCol = lat,
             dateformat = "%d/%m/%Y") 

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c())

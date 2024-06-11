library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/PaysdelaLoire-MNEmayenne"

protocoles <- c("UICN", "LPO", "EGIS", "DREAL", "LGV", "OPP")

dat <- read_sf(dat.filename)

dat[, c("lon", "lat")] <- st_coordinates(dat)

dat <- dat %>%
  st_drop_geometry %>%
  mutate(comment = toupper(paste0(code_etude, " ", comment))) %>%
  addProtocol(
    patterns = c("UICN|PNA|PRA|LPO72|EGIS|DREAL|LGV"),
    protocol = IUCN,
    col1 = comment
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = comment
  ) %>%
  arrangeProtocols(IUCN, PO)

dat <- dat %>%
  formatData(dataSourceStr = "MNE",
             protocolCol = protocol,
             observerCol = observateu,
             dateCol = date_jour,
             presenceCond = is_present > 0,
             xCol = lon,
             yCol = lat,
             dateformat = "%Y-%m-%d") 

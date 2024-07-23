library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/BourgogneFrancheComté-LPOBFA/Copie de Données_loutre_LPOBFC_thèse.csv"
dat <- read.csv(dat.filename, sep = ";")

dat <- dat %>%
  mutate(Remarque = toupper(paste0(Remarque, Protocole))) %>%
  filterCamTrap(col = Remarque) %>%
  addProtocol(
    patterns = c("PRA|PROTOCOLE STANDARD"),
    protocol = IUCN,
    col1 = Remarque) %>% 
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = Remarque) %>%
  arrangeProtocols(IUCN, PO) %>%
  formatData(
    dataSourceStr = "LPO-BFC",
    observerCol = "",
    protocolCol = protocol,
    dateCol = Date,
    presenceCond = sign(Nombre),
    gridCellCol = Maille,
    dateformat = "%d/%m/%Y"
  )

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c())
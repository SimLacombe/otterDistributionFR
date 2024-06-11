library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat1.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/données_loutre_SHNA_brut.csv"
dat2.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/BBF20231005_Loutre_etudeCNRS_SFEPM_2021_2023.csv"


dat1 <- read.csv(dat1.filename, sep = ";")
dat2 <- read.csv(dat2.filename, sep = ";")

dat1 <- dat1 %>%
  filter(LAMBERT_93 != "") %>%
  mutate(comment = toupper(paste0("TYPE:", TYPE, "-", COMMENTAIRE_RELEVE)))

dat2 <- dat2 %>%
  filter(DATE_OBS != "") %>%
  filterCamTrap(col = toupper(METHODE)) %>%
  mutate(comment = toupper(paste0("TYPE:", TYPE, "-", COMMENTAIRE_RELEVE)))

dat1 <- dat1 %>%
  addProtocol(
    patterns = c("TYPE:PONT"),
    protocol = OFABPP,
    col1 = comment) %>% 
  addProtocol(
    patterns = c("MAILLE|\\d\\d\\dM|\\d\\d\\d M|!TYPE:-"),
    protocol = IUCN,
    col1 = comment
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = comment) %>%
  arrangeProtocols(OFABPP, IUCN, PO) %>%
  formatData(
    dataSourceStr = "SHNA-OFAB",
    observerCol = str_split_i(str_split_i(OBSERVATEUR, ",", 1), "/", 1),
    protocolCol = protocol,
    dateCol = DATE_OBS,
    presenceCond = EMPREINTES == "VRAI" | CROTTES == "VRAI",
    gridCellCol = LAMBERT_93,
    dateformat = "%Y-%m-%d"
  )

dat2 <- dat2 %>%
  addProtocol(
    patterns = c("SFEPM-Loutre"),
    protocol = IUCN,
    col1 = PROTOCOLE
  ) %>%
  addProtocol(
    patterns = c("TYPE:PONT|TYPE:BUSE|TYPE:VIADUC"),
    protocol = OFABPP,
    col1 = comment
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = comment) %>%
  arrangeProtocols(IUCN, OFABPP, PO) %>%
  formatData(
    dataSourceStr = "SHNA-OFAB",
    observerCol = str_split_i(str_split_i(OBSERVATEUR, ",", 1), "/", 1),
    protocolCol = protocol,
    dateCol = DATE_OBS,
    presenceCond = VIVANT == "VRAI" |
      EMPREINTES == "VRAI" | CROTTES == "VRAI",
    gridCellCol = LAMBERT_93,
    dateformat = "%Y-%m-%d"
  )

dat <- rbind(dat1, dat2)
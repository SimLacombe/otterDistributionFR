library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOanjou/Copie de Export_données_loutre_SFEPM_CEFE_LPO 24_07_2023.xlsx"

dat <- readxl::read_xlsx(dat.filename)

dat <- dat %>%
  mutate(Remarques = toupper(Remarques)) %>%
  filter(!grepl("ÉCRASÉ|CADAVRE", Remarques)) %>%
  filterCamTrap(Remarques) %>%
  addProtocol(
    patterns = c("PNA|PRA|POINT N|POINT X|POINTX|MAILLE|UICN|IUCN|OA|OUVRAGE|ENS BAUGÉ|COUASNON|DESGRANGES"),
    protocol = IUCN,
    col1 = Remarques
  )%>%
  addProtocol(
    patterns = c("CANOÉ|CANOË|KAYAK|PNR|N2000|NATURA 2000"),
    protocol = KAYAK,
    col1 = Remarques
  )%>%
  addProtocol(
    patterns = c("PROSPECTION_&_OUDON|SBO"),
    protocol = SBO,
    col1 = Remarques
  )%>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = Remarques
  )%>%
  arrangeProtocols(IUCN, KAYAK, SBO, PO)

dat <- dat %>%
  formatData(dataSourceStr = "LPO-Anjou",
             protocolCol = protocol,
             observerCol = paste(Nom, Prénom),
             dateCol = Date,
             presenceCond = Nombre > 0,
             xCol = `X Lambert93 [m]`,
             yCol = `Y Lambert93 [m]`,
             gridCellCol = Maille,
             dateformat = "%Y-%m-%d") 
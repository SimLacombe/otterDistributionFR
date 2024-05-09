library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOsarthe/Export_données_loutre_SFEPM_CEFE_LPO_Sarthe 31_01_2024.xlsx"

dat <- readxl::read_xlsx(dat.filename)

dat <- dat %>% 
  mutate(Remarques = toupper(Remarques))%>%
  addProtocol(
    patterns = c("PRA|PSA|PH|UICN"),
    protocol = IUCN,
    col1 = Remarques
  ) %>%
  addProtocol(
    patterns = c("SUIVI LOUTRE 20|SUIVI LOCAL LOUTRE"),
    protocol = SARSL,
    col1 = Remarques
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = Remarques
  ) %>%
  arrangeProtocols(IUCN, SARSL, PO)


dat <- dat %>%
  formatData(dataSourceStr = "LPO-Sarthe",
             protocolCol = protocol,
             dateCol = Date,
             presenceCond = Nombre > 0,
             xCol = `X Lambert93 [m]`,
             yCol = `Y Lambert93 [m]`,
             gridCellCol = Maille,
             dateformat = "%Y-%m-%d") 
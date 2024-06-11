library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/CentreValdeLoire-sologne-nature-environnement/Export_Loutre_SNE-06072023.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1) 

dat <- dat %>%
  addProtocol(
    patterns = c("acquise sur fonds publics",
                 "2015|2019|2020|2021"),
    protocol = IUCN,
    col1 = `Type de donnée`,
    col2 = Date
  ) %>%
  addProtocol(
    patterns = c("privée|acquise sur fonds publics"),
    protocol = PO,
    col1 = `Type de donnée`
  ) %>%
  arrangeProtocols(IUCN, PO)

dat <- dat %>%
  formatData(dataSourceStr = "SNE",
             observerCol = str_split_i(Observateur, ",", 1),
             protocolCol = protocol,
             dateCol = Date,
             presenceCond = `Statut obs`  == "Présent",
             xCol = `x Lambert93`,
             yCol = `y Lambert93`,
             gridCellCol = `Maille 10 Lambert93`,
             dateformat = "%d/%m/%Y") 
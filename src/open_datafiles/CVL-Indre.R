library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/CentreValdeLoire-Indre-Nature/Export-19-03-2024-27_Loutre_IndreNature.csv"

dat <- read_sf(dat.filename)

dat <- dat %>%
  filterCamTrap(toupper(Prospection)) %>%
  filter(`Cause mort` != "Collision routière") %>%
  formatData(dataSourceStr = "Indre-Nature",
             protocolCol = "PO",
             dateCol = Date,
             presenceCond = `Statut obs` == "Présent",
             xCol = `x Lambert93`,
             yCol = `y Lambert93`,
             dateformat = "%d/%m/%Y")


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
             observerCol = str_split_i(Observateur, ",", 1),
             protocolCol = "PO",
             dateCol = Date,
             presenceCond = `Statut obs` == "Présent",
             xCol = as.numeric(`x Lambert93`),
             yCol = as.numeric(`y Lambert93`),
             dateformat = "%d/%m/%Y")

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c())
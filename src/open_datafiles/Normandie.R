library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN.csv"

dat <- read.csv(dat.filename)

dat <- dat %>% 
  mutate(comm = paste0(toupper(Commentaire.inventaire), " ",
                       toupper(Commentaire.citation))) %>%
  filterCamTrap(comm) %>%
  filter(Comportement.N.1 != "2315/Mort par collision avec véhicule")

dat <- dat %>%
  addProtocol(
    patterns = c("UICN|\\d\\d\\dM|\\d\\d\\d M|KM"),
    protocol = IUCN,
    col1 = comm
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = GMNPP,
    col1 = comm
  ) %>%
  arrangeProtocols(IUCN, GMNPP)

dat <- dat %>%
  formatData(dataSourceStr = "GMN",
             observerCol = "",
             protocolCol = protocol,
             dateCol = Date.observation,
             presenceCond = Comportement.N.1 != "0001/Absence d'indice",
             xCol = Longitude.Lambert.93,
             yCol = Latitude.Lambert.93,
             dateformat = "%Y-%m-%d") 

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c("GMNPP"))
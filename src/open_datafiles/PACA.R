library(tidyverse) 
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/PACA-LPOPACA/Copie de Données LPO PACA.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))


dat <- dat %>%
  mutate(COMMENT = paste0(toupper(COMMENT), " ", toupper(PRIVATE_COMMENT))) %>%
  filterCamTrap(COMMENT) %>%
  addProtocol(
    patterns = c("E.\\d\\dN\\d\\d\\d|E\\d\\dN\\d\\d\\d|PNA"),
    protocol = IUCN,
    col1 = COMMENT
  ) %>%
  addProtocol(
    patterns = c("PROSPECTION_&_DURANCE"),
    protocol = DU,
    col1 = COMMENT
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = COMMENT
  ) %>%
  arrangeProtocols(IUCN, DU, PO)

dat <- dat %>%
  formatData(dataSourceStr = "LPO-PACA",
             observerCol = paste(SURNAME, NAME),
             protocolCol = protocol,
             dateCol = DATE,
             presenceCond = TOTAL_COUNT > 0,
             xCol = COORD_LON_L93,
             yCol = COORD_LAT_L93,
             gridCellCol = GRID_NAME,
             dateformat = "%Y-%m-%d") 
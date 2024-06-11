library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"

  
dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(comm = toupper(paste0(TRA_NAME, "-", TRA_SURNAME, "-", COMMENT))) %>%
  filter(HAS_DEATH_INFO != "Oui",
         !grepl("ROUT|COLLISION|ÉCRASÉ|PERCUTÉ", comm)) %>%
  filterCamTrap(comm) %>%  
  addProtocol(
    patterns = c("DUPÉ|MARAIS BRETON|LOUTRE LITTORAL"),
    protocol = IUCN,
    col1 = comm
  ) %>%
  addProtocol(
    patterns = c("RNRVACHERIE|RNNBA"),
    protocol = VENRN,
    col1 = comm
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = PO,
    col1 = comm
  ) %>%
  arrangeProtocols(IUCN, VENRN, PO)

dat <- dat %>%
  formatData(dataSourceStr = "LPO-Vendée",
             protocolCol = protocol,
             observerCol = paste(TRA_NAME, TRA_SURNAME),
             dateCol = DATE,
             presenceCond = TOTAL_COUNT > 0,
             xCol = COORD_LON_L93,
             yCol = COORD_LAT_L93,
             gridCellCol = GRID_NAME,
             dateformat = "%Y-%m-%d") 
library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/Occitanie-LPOoccitanie/extraction SFEPM Loutre 1980-2022.xlsx"
dat2.filename <- "data/PNA-data/Occitanie-LPOoccitanie/PNAdata.csv"

dat <- readxl::read_xlsx(dat.filename)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat2 <- read.csv(dat2.filename, sep = ";", header = T)

dat <- dat %>%
  filterCamTrap(toupper(Remarque)) %>%
  filter(`Contient des détails mortalité` != "Oui") %>%
  formatData(dataSourceStr = "LPO-Occitanie",
             protocolCol = "PO",
             dateCol = Date,
             presenceCond = Nombre > 0,
             xCol = `X Lambert93 [m]`,
             yCol = `Y Lambert93 [m]`,
             gridCellCol = Maille, 
             dateformat = "%Y-%m-%d") 

dat2 <- dat2 %>%
  formatData(dataSourceStr = "LPO-Occitanie",
             protocolCol = "IUCN",
             dateCol = date,
             presenceCond = presence,
             gridCellCol = grid.cell, 
             dateformat = "%d/%m/%Y") 

dat <- rbind(dat, dat2)
library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat1.filename <- "data/PNA-data/Aquitaine-GREGE/Grege_Données LLU tot_mai2021.csv"
dat2.filename <- "data/PNA-data/Aquitaine-GREGE/Fortuites.csv"
dat3.filename <- "data/PNA-data/Aquitaine-GREGE/Prospections_all.csv"

dat1 <- read.csv(dat1.filename, sep = ";", dec = ",") %>%
  filter(PRESENCE_LLU %in% c("Positif", "Négatif"),
         !((Date == "")&(DATE_ == "")))

dat1$date <- dat1$Date
dat1[(dat1$date == ""), "date"] <- dat1[(dat1$date == ""), "DATE_"]

dat1 <- dat1 %>%
  addProtocol(
    patterns = c("A63 Ondres|A65 Suiv|Euskadour|LGV|A65 2014|BPL"),
    protocol = IUCN,
    col1 = ETUDE
  ) %>%
  addProtocol(
    patterns = character(0),
    protocol = GREGEPP
  ) %>% 
  arrangeProtocols(IUCN, GREGEPP)

dat1 <- dat1 %>%
  formatData(dataSourceStr = "GREGE",
             protocolCol = protocol,
             dateCol = date,
             presenceCond = PRESENCE_LLU  == "Positif",
             xCol = POINT_X,
             yCol = POINT_Y,
             dateformat = "%Y-%m-%d") 

dat2 <- read.csv(dat2.filename, sep = ";", dec = ",") %>%
  filter(NATOBS != "MOR")

dat2 <- dat2 %>%
  addProtocol(
    patterns = character(0),
    protocol = PO
  ) %>% 
  arrangeProtocols(PO)

dat2 <- dat2 %>%
  formatData(dataSourceStr = "GREGE",
             protocolCol = protocol,
             dateCol = DATE_,
             presenceCond = 1,
             xCol = POINT_X,
             yCol = POINT_Y,
             dateformat = "%Y-%m-%d") 
  
dat3 <- read.csv(dat3.filename, sep = ";", dec = ",")
dat3 <- dat3 %>%
  addProtocol(
    patterns = character(0),
    protocol = GREGEPP
  ) %>% 
  arrangeProtocols(GREGEPP)

dat3 <- dat3 %>%
  formatData(dataSourceStr = "GREGE",
             protocolCol = protocol,
             dateCol = DATE_,
             presenceCond = PRESENCE_LLU == "Positif",
             xCol = POINT_X,
             yCol = POINT_Y,
             dateformat = "%Y-%m-%d") 

dat <- rbind(dat1, dat2, dat3)
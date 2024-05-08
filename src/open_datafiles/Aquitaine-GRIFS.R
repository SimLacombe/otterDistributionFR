library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat1.filename <- "data/PNA-data/Aquitaine-GRIFS/placettes_2022-2023.csv"
dat2.filename <- "data/PNA-data/Aquitaine-GRIFS/GRIFS_Point_Loutre_202112-202301.csv"

dat1 <- read.csv(dat1.filename)

dat1 <- dat1 %>%
  addProtocol(
    patterns = character(0),
    protocol = GRIFSPP
  ) %>% 
  arrangeProtocols(GRIFSPP) %>%
  formatData(dataSourceStr = "GRIFS",
             protocolCol = protocol,
             dateCol = date,
             presenceCond = presence == 1 &! is.na(presence),
             xCol = lon,
             yCol = lat,
             dateformat = "%Y-%m-%d") 

dat2 <- read.csv(dat2.filename, sep = "\t")
dat2 <- dat2 %>% 
  filter(!ProcObserv == "trouvé mort (collision routière)")
  
dat2 <- dat2  %>%
  rowwise() %>%
  mutate(
    lon = as.numeric(strsplit(
    str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][1]),
    lat = as.numeric(strsplit(
    str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][2])) %>%
  ungroup

dat2 <- dat2 %>%
  addProtocol(
    patterns = character(0),
    protocol = GRIFSPO
  ) %>% 
  arrangeProtocols(GRIFSPO) %>%
  formatData(dataSourceStr = "GRIFS",
             protocolCol = protocol,
             dateCol = DateDebut,
             presenceCond = StatPresen=="présent",
             xCol = lon,
             yCol = lat,
             gridCellCol = Maille10,
             dateformat = "%Y-%m-%d")

dat <- rbind(dat1, dat2)
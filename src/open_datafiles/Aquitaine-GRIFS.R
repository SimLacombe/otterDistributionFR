library(tidyverse)
library(lubridate)
library(sf)

source("src/functions/cleanData_fcts.R")

dat1.filename <- "data/PNA-data/Aquitaine-GRIFS/placettes_2022-2023.csv"
dat2.filename <- "data/PNA-data/Aquitaine-GRIFS/GRIFS_Point_Loutre_202112-202301.csv"

dat1 <- read.csv(dat1.filename)

dat1 <- dat1 %>%
  formatData(dataSourceStr = "GRIFS",
             protocolCol = "GRIFSPP",
             observerCol = "",
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
  formatData(dataSourceStr = "GRIFS",
             observerCol = str_split_i(Observer, "\\(", 1),
             protocolCol = "PO",
             dateCol = DateDebut,
             presenceCond = StatPresen=="présent",
             xCol = lon,
             yCol = lat,
             gridCellCol = Maille10,
             dateformat = "%Y-%m-%d")

dat <- rbind(dat1, dat2)

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c("GRIFSPP"))
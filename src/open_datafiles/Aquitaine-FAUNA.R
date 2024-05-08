library(lubridate)
library(tidyverse)
library(sf)

source("src/functions/cleanData_fcts.R")

data1.path <- "data/PNA-data/Aquitaine-SINP/Point.csv"
data2.path <- "data/PNA-data/Aquitaine-SINP/Polygon.csv"

metadata.path <- "data/PNA-data/Aquitaine-SINP/Metadonnees.csv"

metadat <- read.csv(metadata.path, sep = "\t") %>%
  rename(IdJdd = IdJeuDonnees)

dat <- rbind(
  read.csv(data1.path, sep = "\t"),
  read.csv(data2.path, sep = "\t")
)

dat <- dat %>%
  st_as_sf(wkt = "GeomWkt") %>%
  left_join(metadat[, c("IdJdd", "NomJeuDonnees", "GestionnaireJdd")])

dat[, c("lon", "lat")] <- dat %>% 
  st_centroid %>%
  st_coordinates

dat <- st_drop_geometry(dat)

dat <- dat %>%
  addProtocol(
    patterns = c("Indépendant|CD 33|CEN Nouvelle-Aquitaine", 
                 "opportuniste|naturaliste|faunistique_&!_inventaire|suivi"),
    protocol = FAUNA1,
    col1 = GestionnaireJdd,
    col2 = NomJeuDonnees
  ) %>%
  addProtocol(
    patterns = c("LPO France"),
    protocol = FAUNA2,
    col1 = GestionnaireJdd
  ) %>% 
  arrangeProtocols(FAUNA1, FAUNA2)

dat <- dat %>%
  formatData(dataSourceStr = "FAUNA",
             protocolCol = protocol,
             dateCol = DateDebut,
             presenceCond = StatPresen == "présent",
             xCol = lon,
             yCol = lat,
             gridCellCol = Maille10,
             dateformat = "%Y-%m-%d")
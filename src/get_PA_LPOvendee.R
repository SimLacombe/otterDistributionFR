library(tidyverse)
library(lubridate)
library(foreach)
library(sf)

rm(list = ls())

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"
datMB.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/données_Maraisbreton/Loutre_MB85.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))
dat <- dat %>% select(-SEARCH_EXPORT_SPECIES_SHEET_USE_POPUP)


datMB <- readxl::read_xlsx(datMB.filename, skip = 1)
names(datMB) <- names(readxl::read_xlsx(datMB.filename, n_max = 1)) 
datMB <- datMB %>% select(-c("PROJECT_CODE", "PROJECT_NAME"))

dat <- dat %>% mutate(isPA = ID_SIGHTING %in% datMB$ID_SIGHTING)

datMB.sp <- datMB %>%
  st_as_sf(coords = c("COORD_LON_L93", "COORD_LAT_L93"), crs = 2154)

map_PdL <- read_sf("data/regions-20180101-shp/") %>%
  filter(code_insee == "52") %>%
  st_transform(crs = 2154) 

ggplot(datMB.sp)+
  geom_sf(data = st_crop(map_PdL, st_bbox(datMB.sp))) +
  geom_sf()

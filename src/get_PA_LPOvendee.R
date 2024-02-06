library(tidyverse)
library(lubridate)
library(foreach)

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"
datMB.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/données_Maraisbreton/Loutre_MB85.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))
dat <- dat %>% select(-SEARCH_EXPORT_SPECIES_SHEET_USE_POPUP)


datMB <- readxl::read_xlsx(datMB.filename, skip = 1)
names(datMB) <- names(readxl::read_xlsx(datMB.filename, n_max = 1)) 
datMB <- datMB %>% select(-c("PROJECT_CODE", "PROJECT_NAME"))

dat <- dat %>% mutate(isPA = ID_SIGHTING %in% datMB$ID_SIGHTING)

library(tidyverse)
library(lubridate)

source("src/functions/cleanData_fcts.R")

dat.filename <- "data/PNA-data/Limousin-GMHL/données Loutres 2021-2023 GMHL pour thèse CEFE.xlsx"
dat2.filename <- "data/PNA-data/Limousin-GMHL/Copie de Données-GMHL.xlsx"

dat <- readxl::read_xlsx(dat.filename)
dat2 <- readxl::read_xlsx(dat2.filename)

dat <- dat%>%
  filterCamTrap(toupper(Remarque)) %>%
  filter(`Contient des détails mortalité` != "Oui") %>%
  formatData(dataSourceStr = "GMHL",
             observerCol = paste(Nom, Prénom),
             protocolCol = "PO",
             dateCol = Date,
             presenceCond = Nombre > 0,
             xCol = `X Lambert93 [m]`,
             yCol = `Y Lambert93 [m]`,
             gridCellCol = Maille, 
             dateformat = "%d/%m/%Y") 

surveys$transect <- append(surveys$transect, c())
surveys$pointwise <- append(surveys$pointwise, c())

# og.time <- min(paste(dat2$`Année,N,24,15`, dat2$`Mois,N,24,15`, "24", sep = "-"))
# dat2 <- dat2%>%
#   mutate(date = as.Date(dat2$`Date,D`-min(`Date,D`), origin = og.time),
#          year = `Année,N,24,15`,
#          presence = sign(`Nombre,N,24,15`),
#          data.provider = "GMHL",
#          PA = TRUE,
#          PA.protocole = "transect",
#          collision = FALSE, 
#          CT.period = NA) %>%
#   rename(lon.l93 = `X Lambert9,N,24,15`,
#          lat.l93 = `Y Lambert9,N,24,15`) %>%
#   mutate(grid.cell = ifelse(lon.l93 >= 1000000,
#                             paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
#                             paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))%>%
#   select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)
# 
# dat <- rbind(dat1, dat2)
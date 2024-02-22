require(tidyverse, lubridate)

dat1.filename <- "data/PNA-data/Limousin-GMHL/données Loutres 2021-2023 GMHL pour thèse CEFE.xlsx"
dat2.filename <- "data/PNA-data/Limousin-GMHL/Copie de Données-GMHL.xlsx"

dat1 <- readxl::read_xlsx(dat1.filename)
dat2 <- readxl::read_xlsx(dat2.filename)

dat1 <- dat1%>%
  mutate(date = as.Date(Date, format = "%d/%m/%Y"),
         year = year(date),
         presence = sign(Nombre),
         region = "Limousin",
         data.provider = "GMHL",
         PNA.protocole = FALSE,
         loc = paste(Commune, `Lieu-dit`, sep = ".")) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

dat2 <- dat2%>%
  mutate(date = NA,
         year = `Année,N,24,15`,
         presence = sign(`Nombre,N,24,15`),
         data.provider = "GMHL",
         PNA.protocole = TRUE,
         loc = NA) %>%
  rename(lon.l93 = `X Lambert9,N,24,15`,
         lat.l93 = `Y Lambert9,N,24,15`) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))%>%
  select(data.provider, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

dat <- rbind(dat1, dat2)
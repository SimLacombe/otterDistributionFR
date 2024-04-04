library(tidyverse)
library(lubridate)

dat.filename <- "data/PNA-data/Occitanie-LPOoccitanie/extraction SFEPM Loutre 1980-2022.xlsx"
dat2.filename <- "data/PNA-data/Occitanie-LPOoccitanie/PNAdata.csv"

dat <- readxl::read_xlsx(dat.filename)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(PA.protocole = NA,
         PA = FALSE,
         collision = `Contient des détails mortalité` == "Oui",
         date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         data.provider = "LPO",
         CT.period = NA) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille)
dat <- dat %>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

dat2 <- read.csv(dat2.filename, sep = ";", header = T) %>%
  mutate(PA.protocole = "transect",
         PA = TRUE,
         collision = FALSE,
         data.provider = "LPO-Occitanie",
         lon.l93 = NA,
         lat.l93 = NA,
         date = as.Date(date, format = "%d/%m/%Y"),
         CT.period = NA) %>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

dat <- rbind(dat, dat2)
require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/Occitanie-LPOoccitanie/extraction SFEPM Loutre 1980-2022.xlsx"

dat <- readxl::read_xlsx(dat.filename)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(PNA.protocole = FALSE,
         date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         region = "Occitanie",
         data.provider = "LPO",
         loc = `Lieu-dit`) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

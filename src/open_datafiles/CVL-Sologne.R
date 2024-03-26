require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/CentreValdeLoire-sologne-nature-environnement/Export_Loutre_SNE-06072023.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1) 

dat <- dat %>%
  mutate(date = as.Date(Date, format = "%d/%m/%Y"),
         year = year(date),
         presence = as.numeric(`Statut obs` == "Présent"),
         data.provider = "SNE", 
         PA = TRUE,
         PA.protocole  = "transect",
         collision = FALSE) %>%
  rename(lon.l93 = `x Lambert93`,
         lat.l93 = `y Lambert93`,
         grid.cell = `Maille 10 Lambert93`,
         loc = Commune) %>%
  select(data.provider,PA, PA.protocole, collision, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

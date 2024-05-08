library(tidyverse)
library(lubridate)

dat.filename <- "data/PNA-data/CentreValdeLoire-sologne-nature-environnement/Export_Loutre_SNE-06072023.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1) 

# dat <- dat %>%
#   addProtocol(
#     patterns = c("acquise sur fonds publics",
#                  "2015|2019|2020|2021"),
#     protocol = IUCN,
#     col1 = `Type de donnée`,
#     col2 = Date
#   ) %>%
#   addProtocol(
#     patterns = c("privée|acquise sur fonds publics"),
#     protocol = SNEPO,
#     col1 = `Type de donnée`
#   )
# 
# dat <- dat %>%
#   arrangeProtocols(IUCN, SNEPO) %>%
#   filter(!is.na(protocol))

dat <- dat %>%
  mutate(date = as.Date(Date, format = "%d/%m/%Y"),
         year = year(date),  
         presence = as.numeric(`Statut obs` == "Présent"),
         data.provider = "SNE", 
         PA = `Type de donnée` == "acquise sur fonds publics" & year %in% c(2015, 2019:2021),
         PA.protocole  = ifelse(PA, "transect", NA),
         collision = FALSE,
         CT.period = NA) %>%
  rename(lon.l93 = `x Lambert93`,
         lat.l93 = `y Lambert93`,
         grid.cell = `Maille 10 Lambert93`) %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)


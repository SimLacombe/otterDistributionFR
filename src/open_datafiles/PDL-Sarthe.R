library(tidyverse)
library(lubridate)

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOsarthe/Export_données_loutre_SFEPM_CEFE_LPO_Sarthe 31_01_2024.xlsx"

protocoles <- c("UICN", "SLL", "SL", "OPP")

dat <- readxl::read_xlsx(dat.filename)

dat <- dat %>% 
  mutate(Remarques = toupper(Remarques),
         UICN = grepl("PRA", Remarques)|grepl("PSA", Remarques)|grepl("PH", Remarques)|grepl("UICN", Remarques),
         SLL = grepl("SUIVI LOCAL", Remarques),
         SL = grepl("SUIVI LOUTRE 20", Remarques),
         OPP = TRUE)


dat <- dat %>%
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  mutate(collision = FALSE,
         date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         data.provider = "LPO-PdL") %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  group_by(Réf, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = (protocole != "OPP"),
         PA.protocole = ifelse(PA, "transect", NA),
         collision = FALSE,
         CT.period = NA) %>%
  ungroup %>% 
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

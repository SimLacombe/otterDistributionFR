library(tidyverse)
library(lubridate)
library(sf)

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOanjou/Copie de Export_données_loutre_SFEPM_CEFE_LPO 24_07_2023.xlsx"
protocoles <- c("COLL", "CT", "KAYAK", "UICN", "OA", "ENS", "PNR", "N2000", "SBO", "ONCFS", "CLCT", "OPP")

dat <- readxl::read_xlsx(dat.filename)

# dat <- dat %>%
#   mutate(Remarques = toupper(Remarques)) %>% 
#   addProtocol(
#     patterns = c("PNA|PRA|POINT N|POINT X|POINTX|MAILLE|UICN|IUCN|OA|OUVRAGE|ENS BAUGÉ|COUASNON|DESGRANGES"),
#     protocol = IUCN,
#     col1 = Remarques
#   )

dat <- dat %>% 
  mutate(Remarques = toupper(Remarques),
         COLL = grepl("ÉCRASÉ", Remarques)|grepl("CADAVRE", Remarques),
         CT = grepl("PIÈGE-PHOTO", Remarques)|grepl("PIÈGE PHOTO", Remarques),
         KAYAK = grepl("CANOÉ", Remarques)|grepl("CANOË", Remarques)|grepl("KAYAK", Remarques),
         UICN = grepl("PNA", Remarques)|grepl("PRA", Remarques)|grepl("UICN", Remarques)|grepl("IUCN", Remarques)|grepl("POINT N", Remarques)|
           grepl("POINT \\d", Remarques)|grepl("POINT\\d", Remarques)|grepl("POINT DE SUIVI", Remarques)|grepl("MAILLE", Remarques),
         OA = grepl(" OA", Remarques)|grepl("OUVRAGE", Remarques),
         ENS = grepl("ENS", Remarques)&(grepl("COUASNON", Remarques)|grepl("BAUGÉ", Remarques)),
         PNR = grepl("PNR", Remarques),
         N2000 = grepl("N2000", Remarques)|grepl("NATURA 2000", Remarques),
         SBO = grepl("PROSPECTION", Remarques)&(grepl("OUDON", Remarques)|grepl("SBO", Remarques)),
         ONCFS = grepl("PROSPECTION", Remarques)&grepl("ONCFS", Remarques),
         CLCT = grepl("COLLECTIVE", Remarques)&grepl("PROSPECTION", Remarques),
         OPP = !CT&!UICN&!KAYAK&!OA&!ENS&!PNR&!N2000&!SBO&!ONCFS&!COLL)


dat <- dat %>% 
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  mutate(date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         data.provider = "LPO-PdL") %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  group_by(Réf, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = (!protocole %in% c("OPP", "CT")),
         PA.protocole = ifelse(protocole %in% c("UICN", "KAYAK"), "transect", ifelse(!protocole %in% c("OPP", "CT", "COLL"), "point", NA)),
         collision = protocole == "COLL",
         CT.period = NA) %>%
  ungroup %>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)


library(tidyverse) 
library(lubridate)

dat.filename <- "data/PNA-data/PACA-LPOPACA/Copie de Données LPO PACA.xlsx"

protocoles <- c("CT", "UICN", "DU", "OPP")

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         data.provider = "LPO-PACA") %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME)

dat <- dat %>% 
  mutate(COMMENT = paste0(toupper(COMMENT), " ", toupper(PRIVATE_COMMENT)),
         CT = grepl("PIÈGE", COMMENT)&!grepl("ENLEVÉ", COMMENT),
         UICN = grepl("E.\\d\\dN\\d\\d\\d", COMMENT)|grepl("E\\d\\dN\\d\\d\\d", COMMENT)|
           grepl("PNA", COMMENT),
         DU = grepl("PROSPECTION", COMMENT)&grepl("DURANCE", COMMENT),
         OPP = TRUE)

dat <- dat %>% 
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  group_by(ID_SIGHTING, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = !protocole %in% c("OPP", "CT"),
         PA.protocole = ifelse(PA, "transect", NA),
         collision = FALSE,
         CT.period = NA) %>%
  ungroup

dat <- dat%>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)


library(tidyverse)
library(lubridate)

dat.filename <- "data/PNA-data/PaysdelaLoire-MNEmayenne"

protocoles <- c("UICN", "LPO", "EGIS", "DREAL", "LGV", "OPP")

dat <- read_sf(dat.filename)

dat[, c("lon.l93", "lat.l93")] <- st_coordinates(dat)

dat <- dat %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))
dat <- dat %>%
  st_drop_geometry %>%
  mutate(comment = toupper(comment),
         code_etude = ifelse(is.na(code_etude), "", code_etude),
         UICN = grepl("PNA", comment)|grepl("UICN", comment)|code_etude == "PRA_Loutre",
         LPO = grepl("LPO72", comment),
         EGIS = grepl("EGIS", comment),
         DREAL = grepl("DREAL", comment),
         LGV = grepl("LGV", comment), 
         OPP = TRUE)
  
dat <- dat %>%
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  mutate(collision = FALSE,
         date = as.Date(date_jour),
         year = year(date),
         presence = sign(is_present),
         data.provider = "MNE") %>%
  group_by(id_synthes, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = (protocole != "OPP"),
         PA.protocole = ifelse(PA, "transect", NA),
         collision = FALSE,
         CT.period = NA) %>%
  ungroup %>% 
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

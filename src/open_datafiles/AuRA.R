library(tidyverse)
library(lubridate)
library(sf)

dat.filename <- "data/PNA-data/RhônesAlpes-LPOAURA&GMA/data_loutre_ORB_AURA.gpkg"

protocoles <- c("CT", "COLL", "UICN", "CPO", "EL", "LC", "PL", "PP", "PCS", "GJN", "RDE", "TR", "OPP")

dat <- read_sf(dat.filename)

dat[, c("lon.l93", "lat.l93")] <- dat %>%
  st_coordinates()

dat <- dat %>% 
  as_data_frame() %>% 
  filter(desc_source == "[LPO] visionature") %>% 
  mutate(year = year(date),
         loc = place,
         presence = as.numeric(is_present),
         data.provider = "LPO-AuRA",
         grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat <- dat %>% 
  mutate(comment = paste0(toupper(comment), " ", toupper(comment_priv))) %>%
  mutate(CT = grepl("PIÈGE PHOTO", comment)|grepl("PIEGE PHOTO", comment)|grepl("PIÈGE-PHOTO", comment),
         COLL = grepl("MORT", comment)&(grepl("ROUT", comment)|grepl("NATIONALE", comment)|grepl("ÉCRAS", comment)|grepl("COLLISION", comment)),
         UICN = grepl("UICN", comment)|grepl("IUCN", comment)|(grepl("PROTOCOL", comment)|grepl("PRA LOUTRE", comment)&
                    !(grepl("NON-PROTOCOL", comment)|grepl("NON PROTOCOL", comment)|grepl("HORS PROTOCOL", comment))),
         CPO = grepl("CPO", comment),
         EL = grepl("ETUDE:LOUTRE0", comment),
         LC = grepl("LOUTRE-CASTOR", comment)|grepl("CASTOR-LOUTRE", comment),
         PL = grepl("PROSPECTION LOUTRE", comment)|grepl("RECHERCHE LOUTRE", comment),
         PP = grepl("PONCTUEL", comment),
         PCS = grepl("PROSPECTION CIBLÉE", comment),
         GJN = grepl("GROUPE JEUNES NATURE", comment),
         RDE = grepl("SUIVI RDE", comment),
         TR = grepl("\\d00 M", comment)|grepl("\\d00M", comment),
         OPP = TRUE)

dat <- dat %>% 
  pivot_longer(cols = protocoles, names_to = "protocole", values_to = "TF") %>%
  group_by(id_synthese, data.provider, year, date, lon.l93, lat.l93, grid.cell, presence) %>%
  summarise(protocole = protocole[TF][1]) %>%
  mutate(PA = !protocole %in% c("OPP", "CT"),
        PA.protocole = ifelse(PA, "transect", NA),
        collision = protocole == "COLL",
        CT.period = NA) %>% 
  ungroup

dat <- dat%>%
  select(data.provider, PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)




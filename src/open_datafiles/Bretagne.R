library(tidyverse)
library(lubridate)
library(sf)

# dat.filename <- "data/PNA-DATA/Bretagne-GMB/Data_Lutra_GMB.csv"
# dat <- read.csv(dat.filename, sep = ";") 
# 
# dat <- dat %>%
#   addProtocol(
#     patterns = c("Inventaire Mammifères semi-aquatiques de l'Atlas|prospections Loutre standardisées|études Loutre GMB"),
#     protocol = IUCN,
#     col1 = jdd_nom
#   ) %>%
#   addProtocol(
#     patterns = c("Données opportunistes|Données faunebretagne"),
#     protocol = GMBPO,
#     col1 = jdd_nom
#   )
# 
# dat <- dat %>% 
#   arrangeProtocols(IUCN, GMBPO) %>%
#   filter(!is.na(protocol))

dat <- dat %>%
  mutate(PA = jdd_nom %in% c("Données publiques de l'Inventaire Mammifères semi-aquatiques de l'Atlas 2010-14",
                                        "Données privées de l'Inventaire Mammifères semi-aquatiques de l'Atlas 2010-14",
                                        "Données publiques prospections Loutre standardisées 2000-",
                                        "Données privées prospections Loutre standardisées 2000-",
                                        "Données études Loutre GMB"),
         opportuniste = grepl("Données opportunistes", jdd_nom) | grepl("Données faunebretagne", jdd_nom)) %>% 
  filter(PA | opportuniste)


dat <- dat %>% 
  mutate(collision = etat_biologique == "Trouvé mort : impact routier",
         PA.protocole = ifelse(PA, "transect", NA),
         date = as.Date(date_debut, format = "%d/%m/%Y"),
         year = year(date),
         presence = as.numeric(statut_observation  == "Présent"),
         data.provider = "GMB",
         CT.period = NA)

dat[, c("lon.l93", "lat.l93")] <- dat %>%
  st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"), crs = 4326) %>%
  st_transform(crs = 2154) %>%
  st_coordinates()


dat <- dat %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat <- dat %>%
  select(data.provider,PA, PA.protocole, collision, year, date, lon.l93, lat.l93, grid.cell, presence, CT.period)

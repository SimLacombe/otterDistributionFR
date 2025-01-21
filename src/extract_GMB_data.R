library(tidyverse)
library(sf)

dat.filename <- "data/PNA-DATA/Bretagne-GMB/Data_Lutra_GMB.csv"
dat <- read.csv(dat.filename, sep = ";")

dat[, c("lon", "lat")] <- dat %>%
  st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"), crs = 4326) %>%
  st_transform(crs = 2154) %>%S
  st_coordinates()

dat <- dat %>%
  filter(etat_biologique != "Trouvé mort : impact routier") %>%
  mutate(Standardized = grepl("Inventaire Mammifères semi-aquatiques de l'Atlas|prospections Loutre standardisées|études Loutre GMB", jdd_nom),
         Opportunistic = grepl("Données opportunistes|Données faunebretagne", jdd_nom)) %>%
  filter(Standardized|Opportunistic) %>%
  mutate(date = date_debut,
         year = year(date),
         presence = statut_observation  == "Présent",
         protocol = ifelse(Standardized, "Standardized", "Opportunistic")) %>%
  mutate(observer = ifelse(protocol == "Opportunistic",
                           ifelse(grepl(" - ", jdd_nom),
                                  str_split_i(jdd_nom, " - ", 2),
                                  str_split_i(jdd_nom, " de ", 2)),
                           NA)) %>% 
  select(protocol, observer, year, date, lon, lat, presence)
  
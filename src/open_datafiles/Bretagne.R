require(tidyverse, lubridate, sf)

dat.filename <- "data/PNA-DATA/Bretagne-GMB/Data_Lutra_GMB.csv"
dat <- read.csv(dat.filename, sep = ";") 

dat <- dat %>%
  mutate(PNA.protocole = jdd_nom %in% c("Données publiques de l'Inventaire Mammifères semi-aquatiques de l'Atlas 2010-14",
                                        "Données privées de l'Inventaire Mammifères semi-aquatiques de l'Atlas 2010-14",
                                        "Données publiques prospections Loutre standardisées 2000-",
                                        "Données privées prospections Loutre standardisées 2000-",
                                        "Données études Loutre GMB"),
         opportuniste = grepl("Données opportunistes", jdd_nom) | grepl("Données faunebretagne", jdd_nom)) %>% 
  filter(PNA.protocole | opportuniste)


dat <- dat %>% 
  mutate(date = as.Date(date_debut, format = "%d/%m/%Y"),
         year = year(date),
         presence = as.numeric(statut_observation  == "Présent"),
         region = "Bretagne", 
         data.provider = "GMB") %>%
  rename(loc = communes)%>%
  filter(PNA.protocole|as.logical(presence))

dat[, c("lon.l93", "lat.l93")] <- dat %>%
  st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"), crs = 4326) %>%
  st_transform(crs = 2154) %>%
  st_coordinates()


dat <- dat %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat <- dat %>%
  select(data.provider, region,PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

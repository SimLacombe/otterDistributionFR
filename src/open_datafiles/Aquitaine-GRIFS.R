require(tidyverse, lubridate, concom)

dat1a.filename <- "data/PNA-data/Aquitaine-GRIFS/dataSIG/Nive_placette_2022.shp"
dat1b.filename <- "data/PNA-data/Aquitaine-GRIFS/dataSIG/Nivelle_placette_2022.shp"
dat1c.filename <- "data/PNA-data/Aquitaine-GRIFS/dataSIG/Saison_placettes_2023.shp"
dat2.filename <- "data/PNA-data/Aquitaine-GRIFS/GRIFS_Point_Loutre_202112-202301.csv"

dat1a <- read_sf(dat1a.filename) 
dat1b <- read_sf(dat1b.filename) 
dat1c <- read_sf(dat1c.filename) 

dat1 <-rbind(dat1a, dat1b, dat1c) %>%
  st_transform(crs = 2154)

dat1_P1 <- dat1 %>%
  select(-c("Passage_2", "Epreinte_2")) %>%
  rename(date = Passage_1,
         presence = Epreinte_1)

dat1_P2 <- dat1 %>% select(-c("Passage_1", "Epreinte_1")) %>%
  rename(date = Passage_2,
         presence = Epreinte_2)

dat1 <- rbind(dat1_P1, dat1_P2)
dat1[, c("lon.l93", "lat.l93")] <- dat1 %>%
  st_coordinates()

dat1 <- dat1%>%
  as.data.frame() %>%
  filter(!is.na(date)&!is.na(presence)) %>% 
  mutate(PA = TRUE, 
         PA.protocole = "point",
         collision = FALSE,
         date = as.Date(date),
         loc = NA,
         data.provider = "GRIFS",
         year = year(date)) %>%
  select(data.provider, PA, PA.protocole, collision, year, date, loc, lon.l93, lat.l93, presence)


dat2 <- read.csv(dat2.filename, sep = "\t")
dat2 <- dat2 %>% 
  mutate(PA.protocole = NA, 
         PA = FALSE,
         collision = ProcObserv == "trouvé mort (collision routière)",
         date = as.Date(DateDebut),
         loc = NomCom,
         presence = as.numeric(StatPresen=="présent"),
         data.provider = "GRIFS",
         grid.cell = Maille10,
         year = year(DateDebut)) %>%
  rowwise() %>%
  mutate(lon.l93 = as.numeric(strsplit(str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][1]),
         lat.l93 = as.numeric(strsplit(str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][2])) %>% 
  select(data.provider, PA, PA.protocole, collision, year, date, loc, lon.l93, lat.l93, presence)

dat1.sf <- dat1%>%
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)
dat2.sf <- dat2%>%
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)

dat <- rbind(dat1, dat2)%>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3)))) %>% 
  select(data.provider, PA, PA.protocole, collision, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

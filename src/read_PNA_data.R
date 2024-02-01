library(tidyverse)
library(lubridate)
library(foreach)
library(sf)

rm(list = ls())

### Read FR. map and get L93 grid ----------------------------------------------

map_FR <- read_sf("data/regions-20180101-shp/") %>%
  filter(!code_insee %in% c("04", "94", "02", "01", "03", "06")) %>%
  rmapshaper::ms_simplify() %>%
  st_transform(crs = 2154) 

grid <- st_make_grid(map_FR, crs = 2154, 
                     cellsize = c(10000,10000),
                     offset = c(99000, 6130000)) %>% 
  st_as_sf

grid <- cbind(grid, st_coordinates(st_centroid(grid))) %>%
  rename(geometry = x,
         lon.l93 = X, lat.l93 = Y) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                           paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                           paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))
          

### Read PNA data --------------------------------------------------------------

otter.dat <- data.frame()

### 1. GMB BRETAGNE ###

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

otter.dat <- rbind(otter.dat, dat)

### 2. SNE CVL-SOLOGNE ###

dat.filename <- "data/PNA-data/CentreValdeLoire-sologne-nature-environnement/Export_Loutre_SNE-06072023.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1) 

dat <- dat %>%
  mutate(date = as.Date(Date, format = "%d/%m/%Y"),
         year = year(date),
         presence = as.numeric(`Statut obs` == "Présent"),
         region = "Centre.Val.de.Loire",
         data.provider = "SNE", 
         PNA.protocole = TRUE) %>%
  rename(lon.l93 = `x Lambert93`,
         lat.l93 = `y Lambert93`,
         grid.cell = `Maille 10 Lambert93`,
         loc = Commune) %>%
  select(data.provider, region,PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 3. GMHL LIMOUSIN ###

dat1.filename <- "data/PNA-data/Limousin-GMHL/données Loutres 2021-2023 GMHL pour thèse CEFE.xlsx"
dat2.filename <- "data/PNA-data/Limousin-GMHL/Copie de Données-GMHL.xlsx"

dat1 <- readxl::read_xlsx(dat1.filename)
dat2 <- readxl::read_xlsx(dat2.filename)

dat1 <- dat1%>%
  mutate(date = as.Date(Date, format = "%d/%m/%Y"),
         year = year(date),
         presence = sign(Nombre),
         region = "Limousin",
         data.provider = "GMHL",
         PNA.protocole = FALSE,
         loc = paste(Commune, `Lieu-dit`, sep = ".")) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

dat2 <- dat2%>%
  mutate(date = NA,
         year = `Année,N,24,15`,
         presence = sign(`Nombre,N,24,15`),
         region = "Limousin",
         data.provider = "GMHL",
         PNA.protocole = FALSE,
         loc = NA) %>%
  rename(lon.l93 = `X Lambert9,N,24,15`,
         lat.l93 = `Y Lambert9,N,24,15`) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))%>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

dat <- rbind(dat1, dat2)

otter.dat <- rbind(otter.dat, dat)

### 4. LPO PACA ###

dat.filename <- "data/PNA-data/PACA-LPOPACA/Copie de Données LPO PACA.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT),
         tr_len = as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\d\\dM"))),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\dM"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d.\\dKM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\dKM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d.\\d KM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d KM")))*1000, tr_len))%>%
  mutate(PNA.protocole = grepl("E\\d\\d\\dN\\d\\d\\d", COMMENT)|grepl("E\\d\\dN\\d\\d\\d", COMMENT)|
           grepl("EO\\d\\dN\\d\\d\\d", COMMENT)|grepl("EN\\d\\d\\dN\\d\\d\\d", PRIVATE_COMMENT)|
           grepl("E\\d\\d\\dN\\d\\d\\d", PRIVATE_COMMENT)|grepl("PNA", COMMENT),
         PNA.protocole = PNA.protocole|tr_len >= 300,
         PNA.protocole = ifelse(is.na(PNA.protocole), FALSE, PNA.protocole)) %>%
  mutate(date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         region = "PACA",
         data.provider = "LPO",
         loc = paste(MUNICIPALITY, PLACE, sep = ".")) %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 5. LPO Anjou ###

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOanjou/Copie de Export_données_loutre_SFEPM_CEFE_LPO 24_07_2023.xlsx"

dat <- readxl::read_xlsx(dat.filename)

dat <- dat %>% 
  mutate(Remarques = toupper(Remarques),
         tr_len = as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d\\dM"))),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\dM"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\dKM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\dKM")))*10000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d KM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d KM")))*1000, tr_len))%>%
  mutate(PNA.protocole = grepl("PNA", Remarques)|grepl("PRA", Remarques)|grepl("UICN", Remarques)|grepl("POINT", Remarques),
         PNA.protocole = PNA.protocole|tr_len >= 300,
         PNA.protocole = ifelse(is.na(PNA.protocole), FALSE, PNA.protocole))%>%
  mutate(date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         region = "Anjou",
         data.provider = "LPO",
         loc = paste(Commune, `Lieu-dit`, sep = ".")) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 6. LPO Vendée ###

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT))%>%
  mutate(PNA.protocole = NAME == "Marais Breton"|TRA_NAME == "Marais Breton", 
         date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         region = "Vendée",
         data.provider = "LPO",
         loc = paste(MUNICIPALITY, PLACE, sep = ".")) %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 6bis. LPO Sarthe

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOsarthe/Export_données_loutre_SFEPM_CEFE_LPO_Sarthe 31_01_2024.xlsx"

dat <- readxl::read_xlsx(dat.filename)


dat <- dat %>% 
  mutate(Remarques = toupper(Remarques),
         tr_len = as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d\\dM"))),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\dM"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\dKM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\dKM")))*10000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d KM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d KM")))*1000, tr_len))%>%
  mutate(PNA.protocole = grepl("PNA", Remarques)|grepl("PRA", Remarques)|grepl("UICN", Remarques)|grepl("POINT", Remarques),
         PNA.protocole = PNA.protocole|tr_len >= 300,
         PNA.protocole = ifelse(is.na(PNA.protocole), FALSE, PNA.protocole))%>%
  mutate(date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         region = "Sarthe",
         data.provider = "LPO",
         loc = paste(Commune, `Lieu-dit`, sep = ".")) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 7. LPO AuRA ###

dat.filename <- "data/PNA-data/RhônesAlpes-LPOAURA&GMA/data_LPOAURA_GMA.csv"

dat <- read.csv(dat.filename)
dat <- dat %>% 
  mutate(PNA.protocole = FALSE, 
         date = NA,
         loc = NA,
         presence = as.numeric(obs=="presence"),
         region = "Auvergne Rhône-Alpes",
         data.provider = "LPO",
         grid.cell = ifelse(X >= 1000000,
                            paste0("E", substr(X,1,3),"N",substr(Y,1,3)),
                            paste0("E0", substr(X,1,2),"N",substr(Y,1,3)))) %>%
  rename(lon.l93 = X,
         lat.l93 = Y,
         year = annee) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)
otter.dat <- otter.dat %>%
  filter(year >= 2000)

### 8. GRIFS Aquitaine ###

dat.filename <- "data/PNA-data/Aquitaine-GRIFS/GRIFS_Point_Loutre_202112-202301.csv"

dat <- read.csv(dat.filename, sep = "\t")
dat <- dat %>% 
  mutate(PNA.protocole = FALSE, 
         date = as.Date(DateDebut),
         loc = NomCom,
         presence = as.numeric(StatPresen=="présent"),
         region = "Aquitaine",
         data.provider = "GRIFS",
         grid.cell = Maille10,
         year = year(DateDebut)) %>%
  rowwise() %>%
  mutate(lon.l93 = strsplit(str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][1],
         lat.l93 = strsplit(str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][2]) %>% 
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 9. LPO Bourgogne Franche-Comté

dat.filename <- "data/PNA-data/BourgogneFrancheComté-LPOBFA/Copie de Données_loutre_LPOBFC_thèse.csv"
dat <- read.csv(dat.filename, sep = ";")

dat$Remarque <- iconv(dat$Remarque, from = "ISO-8859-1", to = "UTF-8")%>%toupper

dat <- dat %>% 
  mutate(PNA.protocole = (Protocole == "Protocole standard maille 10x10km adapt\xe9 \xe0 la Franche-Comt\xe9"|grepl("PRA", Remarque)|grepl("PAR", Remarque)), 
         date = as.Date(Date, format = "%d/%m/%Y"),
         loc = NA,
         presence = sign(Nombre),
         region = "Bourgogne.Franche.Comté",
         data.provider = "LPO",
         grid.cell = Maille,
         year = year(date))

dat <- dat %>%
  left_join(grid[,c("lon.l93", "lat.l93", "grid.cell"), by = "grid.cell"]) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
  
otter.dat <- rbind(otter.dat, dat)
  
### 10. SHNA Bourgogne FrancheComté

dat1.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/données_loutre_SHNA_brut.csv"
dat2.filename <- "data/PNA-data/BourgogneFrancheComté-SHNA/BBF20231005_Loutre_etudeCNRS_SFEPM_2021_2023.csv"

dat1 <- read.csv(dat1.filename, sep = ";") %>%
  filter(LAMBERT_93 != "") %>%
  mutate(PROTOCOLE = "SFEPM-Loutre",
         VIVANT = "FAUX") #for now, I need to find a solution to this issue
dat2 <- read.csv(dat2.filename, sep = ";")

dat <- rbind(dat1[, names(dat1) %in% names(dat2)], dat2[, names(dat2) %in% names(dat1)]) %>% 
  filter(DATE_OBS != "") %>%
  mutate(PNA.protocole = PROTOCOLE == "SFEPM-Loutre", 
         date = as.Date(DATE_OBS),
         loc = COMMUNE,
         presence = as.numeric(VIVANT=="VRAI"|EMPREINTES=="VRAI"|CROTTES=="VRAI"),
         region = "Bourgogne.Franche.Comté",
         data.provider = "SHNA",
         grid.cell = LAMBERT_93,
         year = year(date))

dat <- dat %>%
  left_join(grid[,c("lon.l93", "lat.l93", "grid.cell")], by = "grid.cell") %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 11. LPO Occitanie 

dat.filename <- "data/PNA-data/Occitanie-LPOoccitanie/extraction SFEPM Loutre 1980-2022.xlsx"

dat <- readxl::read_xlsx(dat.filename)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(PNA.protocole = FALSE,
         date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         region = "Occitanie",
         data.provider = "LPO",
         loc = `Lieu-dit`) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

### 12. GMN Normandie 

dat1.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN_20210518.xlsx"
dat2.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN_20231010.xlsx"

dat1 <- readxl::read_xlsx(dat1.filename) %>%
  select(-`N° de parcelle`)

dat2 <- readxl::read_xlsx(dat2.filename) %>%
  rename(ORGANISME = `Nom Observateur`)

dat <- rbind(dat1,dat2) 

dat <- dat %>% 
  mutate(PNA.protocole = TRUE,
         date = as.Date(`Date\r\nobservation`),
         year = year(date),
         presence = as.numeric(`Comportement\r\nN°1` != "0001/Absence d'indice"),
         region = "Normandie",
         data.provider = "GMN") %>%
  rename(loc = `Nom INSEE`,
         lon.l93 = `Longitude Lambert 93`,
         lat.l93 = `Latitude Lambert 93`) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))%>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

otter.dat <- rbind(otter.dat, dat)

# saveRDS(otter.dat, "data/otterDat.rds")
# saveRDS(grid, "data/L9310x10grid.rds")
# saveRDS(map_FR, "data/map_fr.rds")

### Some plots -----------------------------------------------------------------

otter.dat %>%
  filter(year %in% 2009:2023, PNA.protocole|presence, region == "Anjou") %>%
  group_by(period = year %/% 4, grid.cell) %>%
  summarize(cell.status = ifelse(any(PNA.protocole&presence>0), "Présente - protocolé",
                                 ifelse(any(presence > 0),"Présente - opportuniste", 
                                        "Non observée")),
            nsample = n()) %>%
  left_join(grid[,c("geometry", "grid.cell")], by = "grid.cell") %>%
  st_as_sf %>%
  ggplot()+
    geom_sf(data=map_FR)+
    geom_sf(aes(fill = cell.status))+
    scale_fill_manual(name = "", values = c("orange", "lightblue", "darkblue"))+
    theme_bw()+
    theme(legend.position = "bottom")+
  facet_wrap(~paste0(period * 4, " - ", period * 4 + 3))

# otter.dat %>%
#   filter(year %in% 2008:2023) %>%
#   mutate(period = year %/% 4) %>%
#   group_by(period, grid.cell) %>%
#   summarize(cell.status = ifelse(any(PNA.protocole)&any(presence>0), "Présente - protocolé",
#                                  ifelse(any(presence > 0),"Présente - opportuniste", 
#                                         "Non observée")),
#             nsample = n()) %>%
#   left_join(grid[,c("geometry", "grid.cell")], by = "grid.cell") %>%
#   st_as_sf %>%
#   ggplot()+
#   geom_sf(data=map_FR)+
#   geom_sf(aes(fill = cell.status))+
#   scale_fill_manual(name = "", values = c("orange", "lightblue", "darkblue"))+
#   theme_bw()+
#   facet_wrap(~paste0(period * 4, " - ", period * 4 + 3))+
#   theme(legend.position = "bottom")

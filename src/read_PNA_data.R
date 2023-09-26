library(tidyverse)
library(lubridate)
library(foreach)
library(sf)

rm(list = ls())

### Read FR. map and get L93 grid ----------------------------------------------

map_FR <- read_sf("data/shapefiles/regions-20180101-shp/") %>%
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

dat.filename <- "data/PNA-DATA/Bretagne-GMB/Data_Lutra_GMB.xlsx"
dat <- readxl::read_xlsx(dat.filename) %>%
  mutate(PNA.protocole = jdd_nom %in%c("DonnÃ©es publiques de l'Inventaire MammifÃ¨res semi-aquatiques de l'Atlas 2010-14",
                       "DonnÃ©es privÃ©es de l'Inventaire MammifÃ¨res semi-aquatiques de l'Atlas 2010-14",
                       "DonnÃ©es publiques prospections Loutre standardisÃ©es 2000-",
                       "DonnÃ©es privÃ©es prospections Loutre standardisÃ©es 2000-",
                       "DonnÃ©es Ã©tudes Loutre GMB")) %>% 
  mutate(date = as.Date(date_debut),
         year = year(date_debut),
         presence = as.numeric(statut_obs == "PrÃ©sent"),
         region = "Bretagne", 
         data.provider = "GMB") %>%
  rename(grid.cell = CODE10KM,
         loc = communes)

dat <- dat %>%
  left_join(grid[,c("lon.l93", "lat.l93", "grid.cell"), by = "grid.cell"]) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)

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

dat.filename <- "data/PNA-data/Limousin-GMHL/données Loutres 2021-2023 GMHL pour thèse CEFE.xlsx"
dat <- readxl::read_xlsx(dat.filename) %>%
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

otter.dat <- rbind(otter.dat, dat)

### 4. LPO PACA ###

dat.filename <- "data/PNA-data/PACA-LPOPACA/Copie de Données LPO PACA.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT))%>%
  mutate(PNA.protocole = (grepl("E\\d\\d\\dN\\d\\d\\d", COMMENT)|grepl("E\\d\\dN\\d\\d\\d", COMMENT)|
                          grepl("EO\\d\\dN\\d\\d\\d", COMMENT)|grepl("EN\\d\\d\\dN\\d\\d\\d", PRIVATE_COMMENT)|
                          grepl("E\\d\\d\\dN\\d\\d\\d", PRIVATE_COMMENT)|grepl("PNA", COMMENT))&!grepl("HORS", COMMENT))%>%
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
  mutate(Remarques = toupper(Remarques))%>%
  mutate(PNA.protocole = grepl("PNA", Remarques))%>%
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
  mutate(PNA.protocole = FALSE, 
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
  
saveRDS(otter.dat, "data/otterDat.rds")
saveRDS(grid, "data/L9310x10grid.rds")

### Some plots -----------------------------------------------------------------
# 
# otter.dat.sf <- st_as_sf(otter.dat, coords = c("lon.l93", "lat.l93"), crs = 2154)
# 
# ggplot(otter.dat.sf)+
#   geom_sf(data=map_FR)+
#   geom_sf(aes(col = as.factor(presence)))
# 
# otter.dat %>% 
#   mutate(PNA.protocole = ifelse(PNA.protocole, "Standard protocole", "All data")) %>%
#   group_by(region, year, PNA.protocole) %>%
#   summarize(n.grid = length(unique(grid.cell))) %>%
#   ggplot()+
#   geom_line(aes(x=year, y = n.grid, col = region), linewidth = 1)+
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   facet_wrap(~PNA.protocole, nrow = 2)+
#   ggtitle("Number of grid cells visited")
# 
# otter.dat %>%
#   mutate(PNA.protocole = ifelse(PNA.protocole, "Standard protocole", "All data")) %>%
#   group_by(region, year, grid.cell,PNA.protocole) %>%
#   summarize(n.visit = n()) %>%
#   group_by(region, year,PNA.protocole) %>%
#   summarize(n.visit = mean(n.visit)) %>%
#   ggplot()+
#   geom_line(aes(x = year, y = n.visit, color = region), linewidth = 1)+
#   geom_hline(aes(yintercept = 4), linetype = "dashed")+
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   facet_wrap(~PNA.protocole, nrow = 2)+
#   ggtitle("Average number of visits per grid cell")
# 
# nyr <- 5
# otter.gridded <- otter.dat %>%
#   mutate(period = year %/% nyr,
#          period = paste0(period * nyr, " - ", period * nyr + nyr-1)) %>%
#   group_by(data.provider, region, period, grid.cell) %>% 
#   summarize(presence = sign(sum(presence))) %>%
#   left_join(grid[,c("geometry", "grid.cell")], by = "grid.cell") %>% 
#   st_as_sf
# 
# ggplot(otter.gridded)+
#   geom_sf(data=map_FR)+
#   geom_sf(aes(fill = factor(presence, labels = c("unobserved", "present"))))+
#   facet_wrap(~period)+
#   scale_fill_manual(name = "", values = c("darkblue", "lightblue"))+
#   theme_bw()+
#   theme(legend.position = "bottom")
# 
# otter.gridded%>%
#   group_by(grid.cell)%>%
#   arrange(grid.cell, period) %>% 
#   mutate(delta.occ = 2 * presence * lag(presence,1) + presence - lag(presence, 1),
#          new.obs = is.na(delta.occ),
#          delta.occ = ifelse(is.na(delta.occ), 2*presence, delta.occ)) %>%
#   ggplot()+
#   geom_sf(data=map_FR)+
#   geom_sf(aes(fill = factor(delta.occ, labels = c("ext","absent"," col", "present"))))+
#   scale_fill_manual(name = "", values = c("red", "orange", "darkgreen", "lightgreen"))+
#   facet_wrap(~period)+
#   theme_bw()+
#   theme(legend.position = "bottom")
library(tidyverse)
library(purrr)
library(sf)

rm(list = ls())

map.path <- "data/regions-20140306-5m-shp/"
rivers.path <- "data/HydroRIVERS_v10_eu_shp/"
landUse.path <- "data/CLC/"
protectedAreas.path <- "data/protectedAreas/"

TypeOfProtAreas <- c(#"Special Protection Area (Birds Directive)",
                     # "Site of Community Importance (Habitats Directive)",
                     "Parc naturel régional",
                     "Parc national, aire d'adhésion",
                     "Ramsar Site, Wetland of International Importance",
                     "Parc national, zone cœur",
                     # "Arrêté de protection de biotope",
                     "Périmètre de protection d’une réserve naturelle nationale",
                     "Réserve naturelle nationale")
                     # "Terrain acquis par le Conservatoire du Littoral")

### LOAD FR MAP ----------------------------------------------------------------

map_FR <- read_sf(map.path) %>%
  filter(code_insee %in% c("42", "72", "83", "25", "26", "53",
                           "24", "21", "43", "23", "11", "91",
                           "74", "41", "73", "31", "52", "22",
                           "54", "93", "82")) %>%
  rmapshaper::ms_simplify() %>%
  st_transform(crs = 2154)

### CREATE GRID ----------------------------------------------------------------

grid <- st_make_grid(map_FR, crs = 2154,
                     cellsize = c(10000,10000),
                     offset = c(99000, 6130000)) %>%
  st_as_sf

grid <- cbind(grid, st_coordinates(st_centroid(grid))) %>%
  rename(geometry = x,
         lon = X, lat = Y) %>%
  mutate(gridCell = ifelse(lon >= 1000000,
                           paste0("E", substr(lon,1,3),"N",substr(lat,1,3)),
                           paste0("E0", substr(lon,1,2),"N",substr(lat,1,3)))) %>%
  st_join(map_FR[, "code_insee"], largest = TRUE) %>%
  filter(!is.na(code_insee))

grid <- grid %>%
  st_intersection(st_union(map_FR))

### GET RIVERs ----------------------------------------------------------------- 

rivers <- read_sf(rivers.path) %>% 
  filter(ORD_STRA > 1) %>%
  st_transform(crs = 2154) %>%
  st_intersection(st_union(grid))

landscape <- st_intersection(rivers, grid) %>%
  mutate(hydroLen = st_length(geometry),
         ripBuffer = st_area(st_buffer(geometry, 100))) %>%
  st_drop_geometry() %>%
  group_by(gridCell) %>%
  summarize(hydroLen = sum(hydroLen),
            ripBuffer = sum(ripBuffer)) %>% 
  left_join(grid, .) %>% 
  mutate(hydroLen = ifelse(is.na(hydroLen), 0, hydroLen))

### GET RIPARIAN HABITATS ------------------------------------------------------

landUse.filenames <- list.files(landUse.path, full.names = TRUE,
                        recursive = TRUE, pattern = "CLC12_")
landUse.filenames <- landUse.filenames[grep(".shp", landUse.filenames)]

landUse <- map(landUse.filenames, read_sf) %>%
  reduce(rbind)

riparian_habitats <- landUse %>%
  filter(CODE_12 %in% c("311", "312", "313", "321", "322", "323")) %>%
  st_intersection(st_buffer(rivers, 100))

rm(landUse)

landscape <- st_intersection(riparian_habitats, landscape) %>%
  mutate(ripArea = st_area(geometry)) %>%
  st_drop_geometry() %>%
  group_by(gridCell) %>%
  summarize(ripArea = sum(ripArea)) %>% 
  left_join(landscape, .) %>% 
  mutate(ripArea = ifelse(is.na(ripArea), 0, ripArea))

landscape <- landscape %>% 
  mutate(ripProp = as.numeric(ripArea / ripBuffer))%>% 
  mutate(ripProp = ifelse(is.na(ripProp), 0, ripProp))

### GET PROTECTED AREAS --------------------------------------------------------

protectedAreas.filenames <- list.files(path = "data/protectedAreas/", 
                                       pattern = "_shp_", full.names = T)

protectedAreas <- map(protectedAreas.filenames, read_sf, 
                      layer = "WDPA_WDOECM_Aug2024_Public_FRA_shp-polygons") %>%
  reduce(rbind) %>% 
  st_simplify(dTolerance = 10) %>%
  st_transform(crs = 2154) %>%
  filter(ISO3 %in% c("FRA", "FRA;ESP", "FRA;ITA;MCO")) %>%
  filter(DESIG %in% TypeOfProtAreas) 

landscape <- st_intersection(grid, protectedAreas) %>%
  st_drop_geometry() %>%
  group_by(gridCell) %>%
  summarize(protection = DESIG[1]) %>%
  mutate(is.protected = as.numeric(!is.na(protection))) %>%
  left_join(landscape, .)

landscape <- landscape %>%
  select(code_insee, gridCell, lon, lat, hydroLen, ripProp, is.protected)

saveRDS(landscape, "data/landscape.rds")

plot1 <- ggplot(landscape) + 
  geom_sf(aes(fill = hydroLen), col = NA) +
  theme_bw()

plot2 <- ggplot(landscape) + 
  geom_sf(aes(fill = ripProp), col = NA) +
  scale_fill_gradient(low = "#003300", high ="chartreuse4") +
  theme_bw()

plot3 <- ggplot(landscape) + 
  geom_sf(aes(fill = factor(is.protected)), col = NA) +
  scale_fill_manual(values = c("lightgrey", "green")) +
  theme_bw()
  
ggpubr::ggarrange(plot1, plot2, plot3)

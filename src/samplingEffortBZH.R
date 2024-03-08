library(tidyverse)
library(foreach)
library(sf)
library(ggforce)

rm(list = ls())

dat.filename <- "data/PNA-DATA/Bretagne-GMB/Data_Lutra_GMB.csv"

grid <- readRDS("data/L9310x10grid.rds")%>% 
  filter(data_region == "Br") %>%
  st_as_sf(crs = 2154)

dat <- read.csv(dat.filename, sep = ";") 

dat <- dat %>%
  filter(grepl("Données opportunistes", jdd_nom) | grepl("Données faunebretagne", jdd_nom))%>%
  mutate(obs.type = ifelse(etat_biologique %in% c("Trouvé mort", "Trouvé mort : impact routier"), "Trouvé mort", "Observé vivant"))


dat <- dat %>% 
  mutate(date = as.Date(date_debut, format = "%d/%m/%Y"),
         year = year(date),
         presence = as.numeric(statut_observation  == "Présent"))

dat <- dat %>%
  st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"), crs = 4326) %>%
  st_transform(crs = 2154)

dat[, c("lon.l93", "lat.l93")] <- st_coordinates(dat)
dat <- dat %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat <- dat %>%
  select(jdd_nom, obs.type, year, date, lon.l93, lat.l93, grid.cell, presence)

observers <- sort(table(dat$jdd_nom[dat$obs.type == "Observé vivant"]), decreasing = TRUE)
observers.score <- unname(c(observers))[5:14]
observers.name <- names(observers)[5:14]

dat <- dat %>%
  rowwise %>%
  mutate(observer = ifelse(jdd_nom %in% observers.name, paste(str_split(jdd_nom, " ")[[1]][5:6], collapse = "."), "autre"),
         score.obs = ifelse(jdd_nom %in% observers.name, observers.score[which(observers.name == jdd_nom)], NA))

sampl.eff <- dat %>% 
  filter(observer != "autre") %>%
  group_by(observer) %>%
  summarise(centroid = st_centroid(st_union(geometry))) %>%
  st_as_sf(coords = centroid, crs = 2154)

circle.list <- foreach(i = 1:nrow(sampl.eff))%do%{
  rad <- quantile(st_distance(dat[c(dat$observer == sampl.eff$observer[i]),], sampl.eff$centroid[i]), 0.75)
  st_coordinates(sampl.eff$centroid[i] + 
    lapply(seq(0, 2*pi, length.out = 100), FUN = function(x){as.numeric(rad) * c(cos(x), sin(x))}))
}

polygon.list <- lapply(circle.list, function(mat){
  st_polygon(list(mat))
})

sampl.eff$homerange <- st_sfc(polygon.list, crs = 2154)
st_geometry(sampl.eff) <- sampl.eff$homerange

dat$sampl.eff <- sapply(st_intersects(dat, sampl.eff$homerange), length)
table(dat$sampl.eff)
mean(dat$sampl.eff)

rivers <- read_sf("data/CoursEau_53_Bretagne") %>%
  st_zm %>%
  filter(Classe < 4)

ggplot(grid) +
  geom_sf() +
  geom_sf(data = rivers, color = "#154c79")+
  geom_sf(data = sampl.eff, aes(fill = observer), show.legend = F, alpha = 0.25) +
  geom_sf(data = dat %>% filter(observer == "autre"), size = .75, alpha = 0.5) +
  geom_sf(data = dat %>% filter(observer != "autre"), aes(col =  paste0(observer, " (n = ", score.obs, ")")), alpha = 0.5) +
  scale_color_discrete(name = "")+
  theme_bw()


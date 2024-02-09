library(tidyverse)
library(lubridate)
library(foreach)
library(sf)

rm(list = ls())

### Read PNA data --------------------------------------------------------------

src.path <- "src/open_datafiles"

otter.dat <- foreach(src = list.files(src.path, full.names = TRUE),.combine = rbind) %do%{
  source(src)
  dat
}

rm(list = setdiff(ls(), "otter.dat"))

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
          

### Get approx lat/lon based on centroid of grid cell --------------------------

otter.dat[is.na(otter.dat$lat.l93), c("lon.l93", "lat.l93")] <- otter.dat[is.na(otter.dat$lat.l93), "grid.cell"] %>%
  left_join(grid[,c("lon.l93", "lat.l93", "grid.cell")], by = "grid.cell") %>%
  select(lon.l93, lat.l93)

# saveRDS(otter.dat, "data/otterDat.rds")
# saveRDS(grid, "data/L9310x10grid.rds")
# saveRDS(map_FR, "data/map_fr.rds")

### Some plots -----------------------------------------------------------------

otter.dat %>%
  filter(year %in% 2009:2023, PNA.protocole|presence) %>%
  group_by(period = year %/% 4, grid.cell) %>%
  summarize(cell.status = ifelse(any(PNA.protocole&presence>0), "Présente - protocolé",
                                 ifelse(any(presence > 0),"Présente - opportuniste", 
                                        "Non observée - protocolé")),
            nsample = n()) %>%
  left_join(grid[,c("geometry", "grid.cell")], by = "grid.cell") %>%
  st_as_sf %>%
  ggplot()+
    geom_sf(data=map_FR)+
  geom_point(data = otter.dat %>% filter(year %in% 2009:2023, !PNA.protocole&!presence) %>%
               mutate(period = year %/% 4), aes(x=lon.l93, y = lat.l93), alpha = 0.1)+
    geom_sf(aes(fill = cell.status))+
    scale_fill_manual(name = "", values = c("orange", "lightblue", "darkblue"))+
    theme_bw()+
    theme(legend.position = "bottom")+
    facet_wrap(~paste0(period * 4, " - ", period * 4 + 3))

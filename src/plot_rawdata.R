library(tidyverse)
library(sf)

rm(list = ls())

source("src/functions/getReplicates.R")
source("src/functions/plotting_functions.R")

colors_local <- c("plum2", "lightblue", "lightgreen")

### Load data ------------------------------------------------------------------

data.filename <- "data/otterDat.rds"
dataRaw.filename <- "data/otterDatRaw.rds"
grid.filename <- "data/landscape.rds"
effort.filename <- "data/samplingEffort.rds"

otterDat <- readRDS(data.filename)
otterDatRaw <- readRDS(dataRaw.filename)
effort <- readRDS(effort.filename)
landscape <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

paths <- list(National = "out/modFr.RData",
              NW = "out/ModNO.RData",
              Center = "out/ModE.RData",
              SE = "out/ModSE.RData")

envs <- map(paths, get_model)

grids <- map(envs, 3)
grids <- map(grids, add_geom, landscape)

rm(envs)

### Plot full dataset ----------------------------------------------------------

stdSummary <- otterDat %>%
  filter(protocol != "PO") %>% 
  group_by(gridCell) %>%
  summarize(presence = sign(sum(presence))) %>% 
  left_join(landscape %>% select(gridCell)) %>%
  mutate(dataType = "Standardized data") %>%
  st_as_sf(crs = 2154)

oppSummary <- otterDat %>%
  filter(protocol == "PO") %>% 
  group_by(gridCell) %>%
  summarize(presence = sum(presence)) %>% 
  left_join(landscape %>% select(gridCell)) %>%
  mutate(dataType = "Opportunistic data") %>%
  st_as_sf(crs = 2154)

dataSummary <- rbind(stdSummary, oppSummary)

grids[[1]]$region <- case_match(grids[[1]]$code_insee,
                                "11" ~ "IdF",
                                "24" ~ "CvL",
                                c("26", "43") ~ "BFC",
                                c("23", "25") ~ "Nor",
                                c("22", "31") ~ "HdF",
                                c("21", "41", "42") ~ "GE",
                                "52" ~ "PdL",
                                "53" ~ "Bre",
                                c("54", "72", "74") ~ "NAq",
                                c("73", "91") ~ "Occ",
                                c("82", "83") ~ "ARA",
                                "93" ~ "PACA")
regions <- grids[[1]] %>% 
  group_by(region) %>% 
  summarize %>%
  mutate(dataType = "Extent of the local models")

ggplot(landscape) +
  geom_sf(col = NA) + 
  geom_sf(data = regions)+
  geom_sf(data = st_union(grids[[2]])  %>% st_as_sf %>% mutate(dataType = "Extent of the local models"),
          col = colors_local[1], fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[3]])  %>% st_as_sf %>% mutate(dataType = "Extent of the local models"),
          col = colors_local[2], fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[4]])  %>% st_as_sf %>% mutate(dataType = "Extent of the local models"),
          col = colors_local[3], fill = NA, linewidth = 2) +
  geom_sf_text(data = regions, aes(label = region)) +
  geom_sf(data = dataSummary %>% filter(dataType == "Standardized data"),
          aes(fill = factor(presence)))+
  scale_fill_manual(name = " ", labels = c("Non-detection", "Presence"), values = c("orange", "chartreuse4"))+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))+
  ggnewscale::new_scale_fill()+
  geom_sf(data = dataSummary %>% filter(dataType == "Opportunistic data"),
          aes(fill = presence), col = NA) +
  scale_fill_gradient(name = "Number of detections", trans = "log", breaks = c(2.5,10,40,160)) +
  facet_wrap(~dataType) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  xlab("") + ylab("")

### Plot action areas ----------------------------------------------------------

regions <- c("83", "91", "73", "93", "82")
dataSources <- c("LPO-Occitanie", "LPO-PACA", "LPO-AuRA")

otterDatRaw2 <- otterDatRaw %>%
  filter(protocol == "PO")

actionAreas <- otterDatRaw2 %>%
  filter(dataSource %in% dataSources) %>%
  group_by(dataSource) %>% 
  summarize(aa = getSamplingArea(lon, lat, lvl = 0.001, 
                                 offset = st_bbox(landscape2)[c(1,3,2,4)] + c(-1,1,-1,1)*25000)) %>%
  st_as_sf(crs = 2154)

otterDatRaw2 <- otterDatRaw2 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2154)

plot1 <- ggplot(otterDatRaw2 %>% filter(dataSource %in% dataSources))+
  geom_sf(data = landscape %>% filter(code_insee %in% regions) %>% st_union)+
  geom_sf(data = actionAreas, aes(fill = dataSource), alpha = 0.33)+
  geom_sf(aes(col = dataSource), size = 0.25)+
  scale_fill_discrete(name = "")+
  scale_color_discrete(name = "")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

years <- c(2009, 2014, 2019, 2023)

colnames(effort) <- paste0("yr.", 2009:2023)

landscape2 <- cbind(landscape, effort[, years - 2008]) %>%
  pivot_longer(cols = all_of(paste0("yr.", years)), names_to = "year", values_to = "dataSource") %>%
  mutate(year = as.numeric(gsub("\\D", "", year)))

plot2 <- ggplot(landscape2 )+
  geom_sf(data = st_union(landscape2))+
  geom_sf(aes(fill = dataSource), col = NA, show.legend = FALSE)+
  geom_sf(data = otterDatRaw2 %>% filter(year %in% years, presence == 1), size = 0.25)+
  facet_wrap(~year, ncol = 2)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggpubr::ggarrange(plot1, plot2, legend = "bottom")
 
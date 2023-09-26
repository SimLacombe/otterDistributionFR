library(tidyverse)
library(lubridate)
library(foreach)
library(sf)

rm(list = ls())

data.filename <- "data/otterDat.rds"

otterDat <- readRDS(data.filename)

occDat <- otterDat %>% 
  filter(region == "PACA", PNA.protocole) %>% 
  group_by(year, grid.cell, date, loc) %>%
  summarize(presence = sign(sum(presence))) %>%
  group_by(year, grid.cell) %>%
  mutate(visit = 1:n()) %>% 
  arrange(year, grid.cell)

occDat.info <- occDat %>%
  group_by(year, grid.cell) %>% 
  summarize(n.visit = max(visit))


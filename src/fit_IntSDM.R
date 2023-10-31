library(tidyverse)
library(sf)
library(concom)

library(LaplacesDemon)
library(runjags)
library(rjags)
library(coda)

rm(list = ls())

source("src/utility_functions.R")

### Load data ###

data.filename <- "data/otterDat.rds"

otterDat <- readRDS(data.filename) %>% 
  filter(year == 2020) %>%
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)

### Keep only PACA 2020 ###

otterDat <- otterDat %>% 
  filter(region == "PACA")

### Get individual transects ### 

thr.space <- 500
thr.time <- 2

otterDat <- otterDat %>% 
  group_by(PNA.protocole, year, grid.cell) %>%
  mutate(obs = collapse_transects(date, geometry, thr.space, thr.time)) %>% 
  group_by(PNA.protocole, year, grid.cell, obs) %>%
  arrange(desc(presence)) %>%
  filter(row_number()==1) %>% 
  arrange(year, grid.cell) %>%
  filter(obs < 6)

### get gridded landscape ### 

map.filename <- "data/map_fr.rds"
grid.filename <- "data/L9310x10grid.rds"

map <- readRDS(map.filename) %>%
  filter(code_insee == "93")
L93_grid <- readRDS(grid.filename) %>%
  st_intersection(map) %>%
  select(grid.cell)

L93_grid$logArea <- log(as.numeric(st_area(L93_grid)))
L93_grid$intercept <- 1

### Format data to run JAGS mod ### 

pa.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(PNA.protocole) %>%
  group_by(grid.cell) %>%
  summarize(K = n(),
            y = sum(presence))
pa.dat$pixel <- sapply(pa.dat$ grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})

po.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(!PNA.protocole, as.logical(presence)) %>%
  group_by(grid.cell) %>%
  summarize()

po.dat$pixel <- sapply(po.dat$ grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})
po.dat$ones <- 1

### Fit JAGS mod ### 

data.list <- list(cell_area = L93_grid$logArea,
                  npixel = nrow(L93_grid),
                  npo = nrow(po.dat),
                  nsite = nrow(pa.dat),
                  K = pa.dat$K,
                  x_lam =  L93_grid$intercept,
                  x_b = L93_grid$intercept,
                  x_rho = L93_grid$intercept,
                  ncov_lam = 1,
                  ncov_b = 1,
                  ncov_rho = 1,
                  po_pixel = po.dat$pixel,
                  pa_pixel = pa.dat$pixel,
                  y = pa.dat$y,
                  ones = po.dat$ones,
                  cste = 1000)

z0 <- as.numeric(sapply(L93_grid$grid.cell, FUN = function(x){x %in% c(po.dat$grid.cell, pa.dat$grid.cell)}))

inits <- list(list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0),
              list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0),
              list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0),
              list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0))

mod <- run.jags(model = "src/intSDM_JAGSmod.R",
                monitor = c("lambda", "z", "b", "rho", "alpha", "beta", "gamma"),
                data = data.list,
                n.chains = 4,
                inits = inits,
                adapt = 500,
                burnin = 1000,
                sample = 1000,
                thin = 1,
                summarise = TRUE,
                plots = TRUE,
                method = "parallel")
s
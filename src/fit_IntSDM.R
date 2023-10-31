library(tidyverse)
library(sf)
library(concom)

library(LaplacesDemon)
library(runjags)
library(rjags)
library(coda)
library(mcmcplots)

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

### Get gridded landscape ### 

map.filename <- "data/map_fr.rds"
grid.filename <- "data/L9310x10grid.rds"

map <- readRDS(map.filename) %>%
  filter(code_insee == "93")
L93_grid <- readRDS(grid.filename) %>%
  st_intersection(map) %>%
  select(grid.cell)

L93_grid$logArea <- log(as.numeric(st_area(L93_grid))/1000**2)

### Get Spatial covariates ### 

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
                  x_lam =  matrix(L93_grid$intercept, nrow(L93_grid), 1),
                  x_b = matrix(L93_grid$intercept, nrow(L93_grid), 1),
                  x_rho = matrix(L93_grid$intercept, ),
                  ncov_lam = 1,
                  ncov_b = 1,
                  ncov_rho = 1,
                  po_pixel = po.dat$pixel,
                  pa_pixel = pa.dat$pixel,
                  y = pa.dat$y,
                  ones = po.dat$ones,
                  cste = 1000)

z0 <- as.numeric(sapply(L93_grid$grid.cell,
                        FUN = function(x){x %in% c(po.dat$grid.cell, pa.dat$grid.cell)}))

inits <- list(list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0),
              list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0),
              list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0),
              list(beta = 0, alpha = logit(0.5), gamma = logit(0.5), z = z0))

mod <- run.jags(model = "src/intSDM_JAGSmod.R",
                monitor = c("z", "alpha", "beta", "gamma"),
                data = data.list,
                n.chains = 4,
                inits = inits,
                adapt = 500,
                burnin = 5000,
                sample = 5000,
                thin = 1,
                summarise = TRUE,
                plots = TRUE,
                method = "parallel")

denplot(as.mcmc.list(mod), parms= c("alpha","beta", "gamma"), collapse = FALSE)

mod.mat <- as.matrix(as.mcmc.list(mod), chains = T)

z.est <- mod.mat[, grep("z\\[", colnames(mod.mat))] %>% 
  apply(2, mean)

riv_nw <- read_sf("data/eu_riv_30s/") %>%
  st_transform(crs = 2154) %>%
  st_intersection(map)

ggplot(otterDat)+
  geom_sf(data = map)+
  geom_sf(data = L93_grid, aes(fill=z.est), alpha = 0.75) +
  geom_sf(aes(color = factor(presence), pch = PNA.protocole))+
  geom_sf(data = riv_nw, aes(alpha = log(UP_CELLS)), color = "#002266", show.legend = FALSE)+
  scale_color_manual(values = c("red", "blue"))+
  scale_shape_manual(values = c(4,19))+
  facet_wrap(~year)+
  scale_fill_gradient(low = "grey", high = "brown")+
  theme_bw()



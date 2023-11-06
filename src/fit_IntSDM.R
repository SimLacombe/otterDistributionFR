library(tidyverse)
library(foreach)
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
  dplyr::select(grid.cell)

L93_grid$logArea <- log(as.numeric(st_area(L93_grid))/1000**2)

npixel <- nrow(L93_grid)

### Get Spatial covariates ### 

L93_grid$intercept <- 1

### Format data to run JAGS mod ### 

sites <- unique(otterDat$grid.cell)
years <- unique(otterDat$year)

nsite <- length(sites)
nyear <- length(years)

pa.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(PNA.protocole) %>%
  group_by(grid.cell, year) %>%
  summarize(K = n(),
            y = sum(presence)) %>% 
  ungroup

y <- K <- matrix(0, nsite, nyear)
for(s in 1:nsite){
  for (t in 1:nyear){
    y[s,t] <- max(0, pa.dat %>% 
      filter(year == years[t], grid.cell == sites[s]) %>%
      .$y)
    K[s,t] <- max(0, pa.dat %>% 
      filter(year == years[t], grid.cell == sites[s]) %>%
      .$K)
  }
}

pa_pixel <- sapply(sites, FUN = function(x){which(L93_grid$grid.cell == x)})

po.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(!PNA.protocole, as.logical(presence)) %>%
  group_by(year, grid.cell) %>%
  summarize()

po.dat$pixel <- sapply(po.dat$ grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})
po.dat$ones <- 1

npo <- po.dat %>%
  group_by(year) %>%
  summarize(n = n()) %>% 
  .$n

po.idxs <- c(0, cumsum(npo))

### Get Laplacian matrix of the L93 grid ### 

cplx_crd <- complex(real = substr(L93_grid$grid.cell, 2,4),
                             imaginary = substr(L93_grid$grid.cell, 6,8))
D.mat <- sapply(cplx_crd, FUN = function(x){as.numeric(abs(x-cplx_crd))})


### Fit JAGS mod ### 

data.list <- list(cell_area = L93_grid$logArea,
                  nyear= nyear,
                  npixel = npixel,
                  npo = npo,
                  po.idxs = po.idxs,
                  nsite = nsite,
                  x_psi =  matrix(L93_grid$intercept, npixel, 1),
                  x_lam =  matrix(L93_grid$intercept, npixel, 1),
                  x_b = matrix(L93_grid$intercept, npixel, 1),
                  x_rho = matrix(L93_grid$intercept, npixel, 1),
                  ncov_psi = 1,
                  ncov_lam = 1,
                  ncov_b = 1,
                  ncov_rho = 1,
                  po_pixel = po.dat$pixel,
                  pa_pixel = pa_pixel,
                  y = y,
                  K = K,
                  # D = exp(-D.mat),
                  ones = po.dat$ones,
                  cste = 1000)

z0 <- matrix(1, npixel, nyear)

inits <- list(list(beta_psi = logit(0.5), beta_lam = 0, beta_b = logit(0.5),
                   beta_rho = logit(0.5), gamma0 = 0.5, sigma = 1,  z = z0),
              list(beta_psi = logit(0.5), beta_lam = 0, beta_b = logit(0.5),
                   beta_rho = logit(0.5), gamma0 = 0.5, sigma = 1, z = z0),
              list(beta_psi = logit(0.5), beta_lam = 0, beta_b = logit(0.5),
                   beta_rho = logit(0.5), gamma0 = 0.5, sigma = 1, z = z0),
              list(beta_psi = logit(0.5), beta_lam = 0, beta_b = logit(0.5),
                   beta_rho = logit(0.5), gamma0 = 0.5, sigma = 1, z = z0))

mod <- run.jags(model = "src/intSDM2_JAGSmod.R",
                monitor = c("z", "beta_psi", "beta_lam", "beta_b", "beta_rho", "beta_gam"),
                data = data.list,
                n.chains = 4,
                inits = inits,
                adapt = 100,
                burnin = 1000,
                sample = 1000,
                thin = 1,
                summarise = TRUE,
                plots = TRUE,
                method = "parallel")

denplot(as.mcmc.list(mod), parms= c("beta_psi", "beta_lam", "beta_b", "beta_rho", "beta_gam"), collapse = FALSE)

mod.mat <- as.matrix(as.mcmc.list(mod), chains = T)

z.est <- data.frame(z.est = apply(mod.mat[, grep("z\\[", colnames(mod.mat))], 2, mean),
                    year = rep(years, each = npixel),
                    pixel = rep(1:npixel, nyear))
  

riv_nw <- read_sf("data/eu_riv_30s/") %>%
  st_transform(crs = 2154) %>%
  st_intersection(map)

ggplot(otterDat)+
  geom_sf(data = map)+
  geom_sf(data = z.est, aes(geometry = rep(L93_grid$geometry, nyear), fill=z.est), alpha = 0.75) +
  geom_sf(aes(color = factor(presence), pch = PNA.protocole))+
  geom_sf(data = riv_nw, aes(alpha = log(UP_CELLS)), color = "#002266", show.legend = FALSE)+
  scale_color_manual(values = c("red", "blue"))+
  scale_shape_manual(values = c(4,19))+
  facet_wrap(~year)+
  scale_fill_gradient2(low = "firebrick2", mid= "white", high = "springgreen4", midpoint = 0)+
  theme_bw()

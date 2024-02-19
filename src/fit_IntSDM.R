library(tidyverse)
library(foreach)
library(sf)
library(concom)

library(LaplacesDemon)
library(runjags)
library(rjags)
library(coda)
library(mcmcplots)
library(mgcv)

rm(list = ls())

source("src/utility_functions.R")

### Load data ###

data.filename <- "data/otterDat.rds"

otterDat <- readRDS(data.filename) %>% 
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)

### Keep only PACA ###

otterDat <- otterDat %>% 
  filter(year %in% 2009:2023, !is.na(date))

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
grid.filename <- "data/L9310x10grid_KDE.rds"

map <- readRDS(map.filename) %>%
  st_union()

L93_grid.sp <- readRDS(grid.filename) %>%
  st_intersection(map)

L93_grid <- L93_grid.sp %>%
  st_drop_geometry()

L93_grid$logArea <- log(as.numeric(st_area(L93_grid.sp))/1000**2)

npixel <- nrow(L93_grid)

### Get Spatial covariates ### 

L93_grid$intercept <- 1

### Format data to run JAGS mod ### 
tmp.res <- 4 #years

otterDat$period = otterDat$year %/% tmp.res

pa.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(PNA.protocole) %>%
  group_by(period, grid.cell) %>%
  summarize(K = n(),
            y = sum(presence))

pa.dat$pixel <- sapply(pa.dat$grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})

npa <- pa.dat %>%
  group_by(period) %>%
  summarize(n = n()) %>% 
  .$n
pa.idxs <- c(0, cumsum(npa))

po.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(!PNA.protocole, as.logical(presence)) %>%
  group_by(period, grid.cell) %>%
  summarize()

po.dat$pixel <- sapply(po.dat$grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})
po.dat$ones <- 1

npo <- po.dat %>%
  group_by(period) %>%
  summarize(n = n()) %>% 
  .$n
po.idxs <- c(0, cumsum(npo))

nperiod <- length(unique(pa.dat$period))

### Get Laplacian matrix of the L93 grid ### 

# cplx_crd <- complex(real = substr(L93_grid$grid.cell, 2,4),
#                              imaginary = substr(L93_grid$grid.cell, 6,8))
# D.mat <- sapply(cplx_crd, FUN = function(x){as.numeric(abs(x-cplx_crd))})

### Set up the GAM ###

jags.file <- "JAGS/test.jags"

tmpDat <- data.frame(1, st_coordinates(st_centroid(st_transform(L93_grid.sp, crs = 4326))))
names(tmpDat) <- c("y", "E", "N")
  
gamDat <- jagam(y ~ s(E,N, k = 10, bs = "ds", m = c(1,0.5)),
                 data = tmpDat, file = jags.file, 
                 family = "binomial")

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

### Fit JAGS mod ### 

data.list <- list(cell_area = L93_grid$logArea,
                  npixel = npixel,
                  nyear = nperiod,
                  npo = npo,
                  nspline = length(gamDat$jags.data$zero),
                  po.idxs = po.idxs,
                  pa.idxs = pa.idxs,
                  x_lam =  matrix(L93_grid$intercept, npixel, 1),
                  x_thin = matrix(L93_grid$intercept, npixel, 1),
                  x_rho =  matrix(L93_grid$intercept, npixel, 1),
                  sampl_eff = as.matrix(L93_grid[, 2:5]),
                  x_gam = gamDat$jags.data$X,
                  S1 = gamDat$jags.data$S1,
                  ncov_lam = 1,
                  ncov_thin = 1,
                  ncov_rho = 1,
                  po_pixel = po.dat$pixel,
                  pa_pixel = pa.dat$pixel,
                  y = pa.dat$y,
                  K = pa.dat$K,
                  ones = po.dat$ones,
                  zero = gamDat$jags.data$zero,
                  cste = 1000)

source("src/jags_ini.R")
inits <- foreach(i = 1:4)%do%{my_inits(i)}

mod <- run.jags(model = "JAGS/intSDMgam_JAGSmod.R",
                monitor = c("z", "lambda", "beta_lam", "beta_rho", "beta_thin", "b", "lambda_gam"),
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

denplot(as.mcmc.list(mod), parms= c("beta_lam", "beta_rho", "beta_thin", "lambda_gam", "beta_sampl"), collapse = FALSE)

mod.mat <- as.matrix(as.mcmc.list(mod), chains = T)

z.est <- apply(mod.mat[, grep("z\\[", colnames(mod.mat))], 2, mean)
lam.est <- apply(mod.mat[, grep("lambda\\[", colnames(mod.mat))], 2, mean)
lam.sd <- apply(mod.mat[, grep("lambda\\[", colnames(mod.mat))], 2, sd)

lam.est.df <- data.frame(mean.lam = c(lam.est),
                         period = rep(unique(otterDat$period), each = npixel),
                         px = rep(1:npixel, nperiod))

lam.sd.df <- data.frame(lam.sd = c(lam.sd),
                         period = rep(unique(otterDat$period), each = npixel),
                         px = rep(1:npixel, nperiod))

# riv_nw <- read_sf("data/eu_riv_30s/") %>%
#   st_transform(crs = 2154) %>%
#   st_intersection(map)

otterDat <- otterDat%>%
  filter(PNA.protocole|as.logical(presence)) %>% 
  mutate(dataType = ifelse(PNA.protocole&as.logical(presence), "PNA presence",
                           ifelse(PNA.protocole, "PNA absence", "presence Opportuniste")))

ggplot(map)+
  geom_sf()+
  geom_sf(data = lam.est.df, aes(geometry = rep(L93_grid$geometry, nperiod), fill = 1-exp(-mean.lam)), alpha = 0.85) +
  geom_sf(data = otterDat, aes(color = dataType), alpha = 0.5, size = .6)+
  # geom_sf(data = riv_nw, aes(alpha = log(UP_CELLS)), color = "#002266", show.legend = FALSE)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0.5, name = "psi")+
  facet_wrap(~paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))+
  theme_bw()




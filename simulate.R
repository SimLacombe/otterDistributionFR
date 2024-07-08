library(tidyverse)
library(mvtnorm)
library(sf)
library(purrr)
library(foreach)

library(mgcv)
library(rjags)
library(coda)

rm(list = ls())

source("src/functions/simulate_fcts.R")
source("src/functions/jags_ini_bis.R")

years <- 1:5

effort.filename <- "data/samplingEffort.rds"
grid.filename <- "data/L9310x10grid.rds"

grid <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

grid$px <- 1:nrow(grid)
grid$logArea <- log(as.numeric(st_area(grid)) / 1000 ** 2)

### 1. SIMULATE LATENT SPACE ---------------------------------------------------

mu <- list(
  c(0.15,0.7),
  c(0.55,0.4),
  c(0.3,0.3))

sigma <-list(
  c(5000, 2500),
  c(5000, 1000),
  c(2500, 5000))

r <- 2500

simDat <- map(years, grid, .f = function(t, grid){
  grid$year <- t
  grid$lambda <- map(1:3, .f = function(i){
    gen_mvn(grid, mu[[i]], sigma[[i]] + c(r,r) * (t-1), 0)}) %>%
    reduce(`+`)
    
  grid
}) 

simDat <- reduce(simDat, rbind)

### 2. SIMULATE OBSERVATIONS ---------------------------------------------------

effort <- readRDS(effort.filename) 

effort <- effort[, c(1,4,6,8,12)] 

simDat$effort <- c(effort)

b <- 0.5
rho <- 0.5

simDat <- simDat %>% 
  rowwise() %>%
  mutate(psi = 1 - exp(-lambda),
         z = rbinom(1, 1, psi),
         K = rbinom(1, 1, 0.2) * (rpois(1,2) + 1),
         ypa = rbinom(1, K, z * rho),
         ypo = rpois(1, lambda * b * effort)) %>%
  filter(effort>0 | K>0)

### 3. FIT THE MODEL -----------------------------------------------------------

## GAM stuff ## 

NSPLINES = 20

jags.file <- "src/JAGS/test.jags"

tmpDat <- grid %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  data.frame(1, .)

names(tmpDat) <- c("y", "E", "N")

gamDat <- jagam(
  y ~ s(
    E,
    N,
    k = NSPLINES,
    bs = "ds",
    m = c(1, 0.5)
  ),
  data = tmpDat,
  file = jags.file,
  family = "binomial"
)

rm(tmpDat)

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

## Format data list ##

data.list <- list(
  npxt = nrow(simDat),
  pxts_pa = which(simDat$K>0),
  pxts_po = which(simDat$ypo>0),
  pxts_po_no = which(simDat$ypo==0&simDat$effort==1),
  nyear = length(unique(simDat$year)),
  nregion = 1,
  px = simDat$px,
  t = simDat$year,
  region = rep(1, nrow(simDat)),
  ypa = simDat$ypa,
  K = simDat$K,
  ypo = simDat$ypo,
  nprotocols = 1,
  pa_protocol = rep(1, nrow(simDat)),
  ncov_lam = 1,
  ncov_thin = 1,
  ncov_rho = 1,
  cell_area = simDat$logArea,
  x_latent =  matrix(0, nrow(grid), 1),
  x_thin = matrix(1, nrow(grid), 1),
  x_rho =  matrix(0, nrow(grid), 1),
  nspline = length(gamDat$jags.data$zero),
  x_gam = gamDat$jags.data$X,
  S1 = gamDat$jags.data$S1,
  zero = gamDat$jags.data$zero,
  ones = rep(1, nrow(simDat))
)

inits <- foreach(i = 1:4) %do% {
  my_inits(i)
}

## FIT ## 

### Params ---------------------------------------------------------------------

jagsPar <- list(N.CHAINS = 4,
                ADAPT = 500,
                BURNIN = 1000,
                SAMPLE = 1000,
                THIN = 1)

### Call jags ------------------------------------------------------------------

## Integrated Species Distribution Model (PA + PO) ##

mod <- jags.model(
  file = "src/JAGS/JAGSmod_bis.R",
  data = data.list,
  inits = inits,
  n.chains = jagsPar$N.CHAINS,
  n.adapt = jagsPar$ADAPT)

update(mod, jagsPar$BURNIN)

mcmc <- coda.samples(
  mod,
  variable.names = c(
    "z",
    "b",
    "beta_region",
    "beta_latent",
    "beta_rho",
    "beta_rho_protocol",
    "beta_thin",
    "lambda_gam"
  ),
  n.iter = jagsPar$SAMPLE,
  thin = jagsPar$THIN
)

rm(mod, jagsPar, inits)

out <- as.matrix(as.mcmc.list(mcmc), chains = T)

outpath <- paste0("out/","Simulated", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)
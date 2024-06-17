library(tidyverse)
library(sf)
library(foreach)
library(mgcv)

library(rjags)
library(coda)

rm(list = ls())

source("src/functions/jags_ini.R")

# Crop Paris + NE
REGIONS <- c("72", "83", "25", "26", "53",
             "24", "43", "23", "91",
             "74", "73", "52",
             "54", "93", "82")

TIMEPERIOD <- 1 #years

TIMELAG <- TIMEPERIOD - 2009 %% TIMEPERIOD

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA AND COVS ~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Load data ------------------------------------------------------------------

data.filename <- "data/otterDat.rds"
grid.filename <- "data/L9310x10grid.rds"
effort.filename <- "data/samplingEffort.rds"

otterDat <- readRDS(data.filename)

L93_grid <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

effort <- readRDS(effort.filename)

### Adapt effort matrix to sp and tmp resolution -------------------------------

periods <- (2009:2023 + TIMELAG) %/% TIMEPERIOD

effort <- effort[L93_grid$code_insee %in% REGIONS, ]

effort <- sapply(unique(periods), function(p){
  sign(apply(effort[, periods == p], 1,sum))
})

### Filter the region of interest ----------------------------------------------

otterDat <- filter(otterDat, code_insee %in% REGIONS)
L93_grid <- filter(L93_grid, code_insee %in% REGIONS)

### Get offset and spatial covariates ------------------------------------------

L93_grid$intercept <- 1
L93_grid$logArea <- log(as.numeric(st_area(L93_grid)) / 1000 ** 2)

### Get primary period ---------------------------------------------------------

otterDat$period <- (otterDat$year + TIMELAG) %/% TIMEPERIOD
# otterDat$period <- 1

### ~~~~~~~~~~~~~~~~~~~~~~~~~~ FORMAT DATA FOR JAGS ~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Presence-Absence data ------------------------------------------------------

paDat <- otterDat %>%
  filter(protocol != "PO", gridCell %in% L93_grid$gridCell) %>%
  group_by(period, gridCell, protocol) %>%
  summarize(
    K = n(),
    y = sum(presence)
  ) %>%
  ungroup %>%
  mutate(protocol.fact = as.numeric(as.factor(protocol)),
         pixel = pmatch(gridCell, L93_grid$gridCell, duplicates.ok = TRUE)) %>%
  arrange(period)

npa <- unname(c(table(paDat$period)))

paIdxs <- c(0, cumsum(npa))

### Presence-Only data ---------------------------------------------------------

poDat <- otterDat %>%
  filter(protocol  == "PO",
         gridCell %in% L93_grid$gridCell) %>%
  select(period, gridCell) %>%
  mutate(pixel = pmatch(gridCell, L93_grid$gridCell, duplicates.ok = TRUE)) %>% 
  arrange(period)

poDat$ones <- 1

npo <- unname(c(table(poDat$period)))

poIdxs <- c(0, cumsum(npo))

### GAM Data -------------------------------------------------------------------

NSPLINES = 20

jags.file <- "src/JAGS/test.jags"

tmpDat <- L93_grid %>%
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

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

### Format data list -----------------------------------------------------------

npixel <- nrow(L93_grid)
nperiod <- length(unique(paDat$period))

data.list <- list(
  cell_area = L93_grid$logArea,
  npixel = npixel,
  nyear = nperiod,
  nregion = length(unique(L93_grid$code_insee)),
  nprotocols = length(unique(paDat$protocol.fact)),
  nspline = length(gamDat$jags.data$zero),
  npo = npo,
  po.idxs = poIdxs,
  pa.idxs = paIdxs,
  ncov_lam = 1,
  ncov_thin = 1,
  ncov_rho = 1,
  x_latent =  matrix(0, npixel, 1),
  x_thin = matrix(L93_grid$intercept, npixel, 1),
  x_rho =  matrix(L93_grid$intercept, npixel, 1),
  x_gam = gamDat$jags.data$X,
  S1 = gamDat$jags.data$S1,
  region = as.numeric(as.factor(L93_grid$code_insee)),
  effort = effort,
  pa_protocole = paDat$protocol.fact,
  po_pixel = poDat$pixel,
  pa_pixel = paDat$pixel,
  y = paDat$y,
  K = paDat$K,
  ones = poDat$ones,
  zero = gamDat$jags.data$zero,
  cste = 1000
)

inits <- foreach(i = 1:4) %do% {
  my_inits(i)
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ RUN MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Params ---------------------------------------------------------------------

N.CHAINS = 4

ADAPT = 500
BURNIN = 1000
SAMPLE = 1000
THIN = 1

### Call jags ------------------------------------------------------------------

## Integrated Species Distribution Model (PA + PO) ##

mod <- jags.model(
  file = "src/JAGS/IntSDMgam_JAGSmod.R",
  data = data.list,
  inits = inits,
  n.chains = N.CHAINS,
  n.adapt = ADAPT)

update(mod, BURNIN)

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
  n.iter = SAMPLE,
  thin = THIN
)

out <- as.matrix(as.mcmc.list(mcmc), chains = T)

outpath <-
  paste0("out/",
         paste(Sys.Date(), paste(REGIONS, collapse = "."), TIMEPERIOD,
               sep = "_"),
         "yrs.rds")
saveRDS(out, outpath)

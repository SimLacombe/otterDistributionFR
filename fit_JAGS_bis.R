library(tidyverse)
library(sf)
library(foreach)
library(mgcv)

library(rjags)
library(coda)

rm(list = ls())

source("src/functions/jags_ini_bis.R")

# Crop Paris + NE
# REGIONS <- c("72", "83", "25", "26", "53",
#              "24", "43", "23", "91",
#              "74", "73", "52",
#              "54", "93", "82")

# Br + PdL + Centre + Basse Normandie
REGIONS <- c("52", "53"," 24", "25")

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

effort <- effort[L93_grid$code_insee %in% REGIONS, ]
colnames(effort) <- paste0("yr.", 2009:2023)

### Filter the region of interest ----------------------------------------------

otterDat <- filter(otterDat, code_insee %in% REGIONS)
L93_grid <- filter(L93_grid, code_insee %in% REGIONS)

### Get offset and spatial covariates ------------------------------------------

L93_grid$logArea <- log(as.numeric(st_area(L93_grid)) / 1000 ** 2)

### Get pixel identifiers ------------------------------------------------------

L93_grid$px <- 1:nrow(L93_grid)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~ FORMAT DATA FOR JAGS ~~~~~~~~~~~~~~~~~~~~~~~~ ###

ISDM_dat <- cbind(st_drop_geometry(L93_grid), effort) %>%
  pivot_longer(cols = all_of(paste0("yr.", 2009:2023)),
               names_to = "year", values_to = "is_po_sampled") %>%
  arrange(year, px)

rm(effort)

### Presence-Absence data ------------------------------------------------------

ISDM_dat <- otterDat %>%
  filter(protocol != "PO", gridCell %in% L93_grid$gridCell) %>%
  group_by(year, gridCell, protocol) %>%
  summarize(
    K = n(),
    ypa = sum(presence)
  ) %>%
  mutate(year = paste0("yr.", year)) %>%
  left_join(ISDM_dat, .)

### Presence-Only data ---------------------------------------------------------

ISDM_dat <- otterDat %>%
  filter(protocol  == "PO",
         gridCell %in% L93_grid$gridCell) %>%
  group_by(year, gridCell) %>%
  summarize(ypo = n()) %>%
  mutate(year = paste0("yr.", year)) %>%
  left_join(ISDM_dat, .)

### remove all NAs -------------------------------------------------------------

ISDM_dat <- modify(ISDM_dat, ~ ifelse(is.na(.x), 0, .x))

### arrange --------------------------------------------------------------------

ISDM_dat <- ISDM_dat %>%
  filter(is_po_sampled|sign(K)) %>% 
  mutate(t = as.numeric(as.factor(year)),
         protocol = as.numeric(as.factor(protocol))) %>%
  select(px, t, is_po_sampled, K, ypa, protocol, ypo, logArea)

###  plot extent ---------------------------------------------------------------

# ggplot(ISDM_dat) + 
#   geom_sf(aes(fill = is_po_sampled)) + 
#   facet_wrap(~year)

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

rm(tmpDat)

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

### Format data list -----------------------------------------------------------

data.list <- list(
  npxt = nrow(ISDM_dat),
  pxts_pa = which(ISDM_dat$K>0),
  pxts_po = which(ISDM_dat$ypo>0),
  pxts_po_no = which(ISDM_dat$ypo==0&ISDM_dat$is_po_sampled==1),
  nyear = length(unique(ISDM_dat$t)),
  px = ISDM_dat$px,
  t = ISDM_dat$t,
  ypa = ISDM_dat$ypa,
  K = ISDM_dat$K,
  ypo = ISDM_dat$ypo,
  is_po_sampled = ISDM_dat$is_po_sampled,
  nprotocols = length(unique(ISDM_dat$protocol)),
  pa_protocol = ISDM_dat$protocol,
  ncov_lam = 1,
  ncov_thin = 1,
  ncov_rho = 1,
  cell_area = ISDM_dat$logArea,
  x_latent =  matrix(0, nrow(L93_grid), 1),
  x_thin = matrix(1, nrow(L93_grid), 1),
  x_rho =  matrix(0, nrow(L93_grid), 1),
  nspline = length(gamDat$jags.data$zero),
  x_gam = gamDat$jags.data$X,
  S1 = gamDat$jags.data$S1,
  zero = gamDat$jags.data$zero,
  ones = rep(1, nrow(ISDM_dat))
)

inits <- foreach(i = 1:4) %do% {
  my_inits(i)
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ RUN MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Params ---------------------------------------------------------------------

jagsPar <- list(N.CHAINS = 4,
               ADAPT = 50,
               BURNIN = 1000,
               SAMPLE = 1000,
               THIN = 1)

### Call jags ------------------------------------------------------------------

system.time(mod <- jags.model(
  file = "src/JAGS/JAGSmod_bis.R",
  data = data.list,
  inits = inits,
  n.chains = jagsPar$N.CHAINS,
  n.adapt = jagsPar$ADAPT))

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

outpath <- paste0("out/","Mod_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)

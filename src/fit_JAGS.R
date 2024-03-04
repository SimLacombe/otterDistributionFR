library(tidyverse)
library(foreach)
library(sf)

library(LaplacesDemon)
library(runjags)
library(rjags)
library(coda)
library(mgcv)

rm(list = ls())

# regions = c("11", "32", "75", "28", "52", "24", "44", "93", "53", "27", "76", "84")
# 11 : Ile de France, 32 : Hauts de France, 75: Nouvelle Aquitaine, 28: Normandie,
# 52 : Pays de la Loire, 24 : Centre Val de Loire, 44 : Grand Est, 93 : PACA,
# 53 : Bretagne, 27 : Bourgogne Franche-comté, 76 : Occitanie, 84 : Auvergne Rhône-Alpes
 
# Bretagne - Pays de la Loire - Normandie : 
# regions = c("28", "53", "52")

# BFC - AuRA - PACA - Occitanie : 
# regions = c("27", "84", "93", "76")

regions = c("11", "32", "75", "28", "52", "24", "44", "93", "53", "27", "76", "84")

tmp.res = 2 #years

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA AND COVS ~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Load data ------------------------------------------------------------------

data.filename <- "data/otterDatFiltered.rds"
grid.filename <- "data/L9310x10grid.rds"
# grid.filename <- "data/L9310x10grid_KDE.rds"

otterDat <- readRDS(data.filename) %>% 
  st_as_sf(crs = 2154)

L93_grid <- readRDS(grid.filename) %>% 
  st_as_sf(crs = 2154)

### Filter the region of interest ----------------------------------------------

otterDat <- filter(otterDat, code_insee %in% regions)
L93_grid <- filter(L93_grid, code_insee %in% regions)

### Get offset and spatial covariates ------------------------------------------

L93_grid$intercept <- 1
L93_grid$logArea <- log(as.numeric(st_area(L93_grid))/1000**2)

### Get primary period ---------------------------------------------------------

otterDat$period <- otterDat$year %/% tmp.res
# otterDat$period <- 1

### ~~~~~~~~~~~~~~~~~~~~~~~~~~ FORMAT DATA FOR JAGS ~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Presence-Absence data ------------------------------------------------------

pa.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(PNA.protocole, grid.cell %in% L93_grid$grid.cell) %>%
  group_by(period, grid.cell) %>%
  summarize(K = n(),
            y = sum(presence)) %>%
  arrange(period)

pa.dat$pixel <- sapply(pa.dat$grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})

npa <- unname(c(table(pa.dat$period)))

pa.idxs <- c(0, cumsum(npa))

### Presence-Only data ---------------------------------------------------------

po.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(!PNA.protocole, as.logical(presence), grid.cell %in% L93_grid$grid.cell) %>%
  group_by(period, grid.cell) %>%
  summarize() %>%
  arrange(period)

po.dat$pixel <- sapply(po.dat$grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})
po.dat$ones <- 1

npo <- unname(c(table(po.dat$period)))

po.idxs <- c(0, cumsum(npo))

### GAM Data -------------------------------------------------------------------

NSPLINES = 20
  
jags.file <- "JAGS/test.jags"

tmpDat <- L93_grid %>% 
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  data.frame(1, .)

names(tmpDat) <- c("y", "E", "N")
  
gamDat <- jagam(y ~ s(E,N, k = NSPLINES, bs = "ds", m = c(1,0.5)),
                 data = tmpDat, file = jags.file, 
                 family = "binomial")

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

### Format data list -----------------------------------------------------------

npixel <- nrow(L93_grid)
nperiod <- length(unique(pa.dat$period))

data.list <- list(cell_area = L93_grid$logArea,
                  npixel = npixel,
                  nyear = nperiod,
                  npo = npo,
                  nspline = length(gamDat$jags.data$zero),
                  po.idxs = po.idxs,
                  pa.idxs = pa.idxs,
                  x_latent =  matrix(0, npixel, 1),
                  x_thin = matrix(L93_grid$intercept, npixel, 1),
                  x_rho =  matrix(L93_grid$intercept, npixel, 1),
                  # sampl_eff = as.matrix(L93_grid[, 2:5]),
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

source("JAGS/jags_ini.R")
inits <- foreach(i = 1:4)%do%{my_inits(i)}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ RUN MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Params ---------------------------------------------------------------------

N.CHAINS = 4

ADAPT = 500
BURNIN = 1000
SAMPLE = 1000
THIN = 1 
  
### Call jags ------------------------------------------------------------------

## Integrated Species Distribution Model (PA + PO) ##

MOD <- "ISDM"
mod <- run.jags(model = "JAGS/intSDMgam_JAGSmod.R",
                monitor = c("z", "psi", "beta_latent", "beta_rho", "beta_thin", "b", "lambda_gam"),
                data = data.list,
                inits = inits,
                n.chains = N.CHAINS,
                adapt = ADAPT,
                burnin = BURNIN,
                sample = SAMPLE,
                thin = THIN,
                summarise = TRUE,
                plots = TRUE,
                method = "parallel")

## Occupancy Model (PA) ##

# MOD <- "occu"
# mod <- run.jags(model = "JAGS/occu.R",
#                 monitor = c("z","psi", "beta_latent", "beta_rho", "b", "lambda_gam"),
#                 data = data.list,
#                 inits = inits,
#                 n.chains = N.CHAINS,
#                 adapt = ADAPT,
#                 burnin = BURNIN,
#                 sample = SAMPLE,
#                 thin = THIN,
#                 summarise = TRUE,
#                 plots = TRUE,
#                 method = "parallel")

out <- as.matrix(as.mcmc.list(mod), chains = T)

outpath <- paste0("outMod/", paste(Sys.Date(), MOD, paste(regions, collapse = "."), tmp.res, sep = "_"), "yrs.rds")
saveRDS(out, outpath)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT OUTPUT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Occupied cells -------------------------------------------------------------

zzz <- out[, grep("z\\[", colnames(out))]
zz <- array(NA, dim = c(nrow(zzz), ncol(zzz)/nperiod, nperiod))
for(t in 1:nperiod){
  zz[,,t] <- zzz[1:nrow(zzz), ((t-1) * ncol(zzz) / nperiod + 1):(t*ncol(zzz)/nperiod)]
}
z <- apply(zz, c(2,3), mean)

rm("zzz", "zz")

z.df <- data.frame(grid.cell = rep(L93_grid$grid.cell, nperiod),
                     meanz = c(z[, 1:nperiod]),
                     period = rep(unique(po.dat$period), each = nrow(z))) %>%
  left_join(L93_grid, .)

z.df$z.occupied <- ifelse(z.df$meanz > 0.25, "OCCUPIED", "UNOCCUPIED")

### Psi ------------------------------------------------------------------------

lll <- out[, grep("psi\\[", colnames(out))]
ll <- array(NA, dim = c(nrow(lll), ncol(lll)/nperiod, nperiod))
for(t in 1:nperiod){
  ll[,,t] <- lll[1:nrow(lll), ((t-1) * ncol(lll) / nperiod + 1):(t*ncol(lll)/nperiod)]
}
l <- apply(ll, c(2,3), mean)

rm("lll", "ll")

latent.df <- data.frame(grid.cell = rep(L93_grid$grid.cell, nperiod),
                   psi = c(l[, 1:nperiod]),
                   period = rep(unique(po.dat$period), each = nrow(l))) %>%
  left_join(L93_grid, .)

### Plot -----------------------------------------------------------------------

otterDat.toplot <- otterDat%>%
  filter(PNA.protocole|as.logical(presence)) %>% 
  mutate(dataType = ifelse(PNA.protocole&as.logical(presence), "PNA presence",
                           ifelse(PNA.protocole, "PNA absence", "presence Opportuniste")))

ggplot()+
  geom_sf(data = latent.df, aes(fill = psi), alpha = .85) +
  geom_sf(data = otterDat.toplot, aes(color = dataType), alpha = .5, size = .6)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", midpoint = .5) +
  facet_wrap(~paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))+
  theme_bw()

 ggplot()+
  geom_sf(data = z.df, aes(fill = z.occupied), alpha = .85) +
  geom_sf(data = otterDat.toplot, aes(color = dataType), alpha = .5, size = .6)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("orange", "white")) +
  facet_wrap(~paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))+
  theme_bw()

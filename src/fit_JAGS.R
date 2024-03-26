library(tidyverse)
library(foreach)
library(sf)

library(LaplacesDemon)
library(runjags)
library(rjags)
library(coda)
library(mgcv)

rm(list = ls())

# regions = c("Aq", "Au", "Bo", "Br", "Cvl", "FC", "Li", "No", "Oc", "PACA", "PdL", "RA", "noDat")canc

regions = c("Au", "Aq","Bo", "FC", "Li", "Oc", "PACA", "RA")

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

otterDat <- filter(otterDat, data_region %in% regions)
L93_grid <- filter(L93_grid, data_region %in% regions)

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
  filter(PA, grid.cell %in% L93_grid$grid.cell) %>%
  group_by(period, grid.cell, PA.protocole) %>%
  summarize(K = n(),
            y = sum(presence),
            protocole = 2 - as.numeric(PA.protocole[1] == "transect")) %>%
  arrange(period)

pa.dat$pixel <- sapply(pa.dat$grid.cell, FUN = function(x){which(L93_grid$grid.cell == x)})

npa <- unname(c(table(pa.dat$period)))

pa.idxs <- c(0, cumsum(npa))

### Presence-Only data ---------------------------------------------------------

po.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(!PA, as.logical(presence), grid.cell %in% L93_grid$grid.cell) %>%
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
                  nregion = length(unique(L93_grid$data_region)),
                  po.idxs = po.idxs,
                  pa.idxs = pa.idxs,
                  x_latent =  matrix(0, npixel, 1),
                  x_thin = matrix(L93_grid$intercept, npixel, 1),
                  x_rho =  matrix(L93_grid$intercept, npixel, 1),
                  region = as.numeric(as.factor(L93_grid$data_region)),
                  x_gam = gamDat$jags.data$X,
                  S1 = gamDat$jags.data$S1,
                  ncov_lam = 1,
                  ncov_thin = 1,
                  ncov_rho = 1,
                  po_pixel = po.dat$pixel,
                  pa_pixel = pa.dat$pixel,
                  pa_protocole = pa.dat$protocole,
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
                monitor = c("z", "beta_region", "beta_latent", "beta_rho","beta_rho_point", "beta_thin", "b", "lambda_gam"),
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
  left_join(L93_grid, .) %>% 
  mutate(period = paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))

z.df$z.occupied <- ifelse(z.df$meanz > 0.25, "OCCUPIED", "UNOCCUPIED")

### Psi ------------------------------------------------------------------------

bbb <- out[, grep("b\\[", colnames(out))]
ll <- array(NA, dim = c(nrow(bbb), nrow(L93_grid), nperiod))

for(i in 1:nrow(ll)){
  ll[i,,] <- gamDat$jags.data$X %*% matrix(bbb[i,], nrow = NSPLINES, ncol = nperiod) + matrix(rep(L93_grid$logArea, nperiod), nrow = nrow(L93_grid), ncol = nperiod) 
}

l <- exp(apply(ll, c(2,3), mean))

sum.l <-  exp(apply(ll, 3, FUN = function(x){quantile(x, c(0.25,0.5,0.75))}))

rm("bbb", "ll")

latent.df <- data.frame(grid.cell = rep(L93_grid$grid.cell, nperiod),
                   psi = 1-exp(-c(l)),
                   period = rep(unique(po.dat$period), each = nrow(l))) %>%
  left_join(L93_grid, .)%>% 
  mutate(period = paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))

latent.df.peryear <- data.frame(period = unique(po.dat$period),
                                psi.inf = 1-exp(-sum.l[1,]),
                                psi.med = 1-exp(-sum.l[2,]),
                                psi.sup = 1-exp(-sum.l[3,]))%>% 
  mutate(period = paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))

### Region x year --------------------------------------------------------------

beta_reg <- out[, grep("beta_region\\[", colnames(out))]
sum.beta_reg <- apply(beta_reg, 2, FUN = function(x){quantile(x, c(0.25,0.5,0.75))})
beta_reg.df <- data.frame(period = rep(unique(po.dat$period), each = length(unique(regions))),
                          region = rep(unique(regions), nperiod),
                          beta_reg.inf = sum.beta_reg[1,],
                          beta_reg.med = sum.beta_reg[2,],
                          beta_reg.sup = sum.beta_reg[3,]) %>% 
  mutate(period = paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))

### Protocoles -----------------------------------------------------------------

beta_rho <- out[, grep("beta_rho", colnames(out))]

rho.df <- data.frame(protocole = rep(c("transect", "point"), each = nrow(beta_rho)),
                     rho = c(invlogit(beta_rho[,1]), invlogit(beta_rho[,1] + beta_rho[,2])))


### Plots ----------------------------------------------------------------------

otterDat.toplot <- otterDat%>%
  filter(PA|as.logical(presence)) %>% 
  mutate(dataType = ifelse(PA&as.logical(presence), "PNA presence",
                           ifelse(PA, "PNA absence", "presence Opportuniste")),
         period = paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))

ggplot()+
  geom_sf(data = latent.df, aes(fill = psi), alpha = .85) +
  geom_sf(data = otterDat.toplot, aes(color = dataType), alpha = .5, size = .6)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", midpoint = .5) +
  facet_wrap(~period)+
  theme_bw()

 ggplot()+
  geom_sf(data = z.df, aes(fill = z.occupied), alpha = .85) +
  geom_sf(data = otterDat.toplot, aes(color = dataType), alpha = .5, size = .6)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("orange", "white")) +
  facet_wrap(~period)+
  theme_bw()
 
 ggplot(latent.df.peryear)+
   geom_point(aes(x = period, y = psi.med))+
   geom_ribbon(aes(x = period, ymin = psi.inf, ymax = psi.sup), alpha = 0.5, col = "black", fill = NA)+
   theme_bw()
 
 ggplot(beta_reg.df)+
   geom_pointrange(aes(x = region, y = beta_reg.med, ymin = beta_reg.inf, ymax = beta_reg.sup, color = region),
                   show.legend = F)+
   geom_hline(aes(yintercept = 0))+
   theme_bw()+
   facet_wrap(~period)

 
 ggplot(rho.df)+
   geom_violin(aes(x = protocole, y = rho, fill = protocole, group = protocole),
                   show.legend = F)+
   geom_hline(aes(yintercept = 0))+
   theme_bw()
 
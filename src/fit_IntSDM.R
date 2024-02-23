library(tidyverse)
library(foreach)
library(sf)

library(LaplacesDemon)
library(runjags)
library(rjags)
library(coda)
library(mcmcplots)
library(mgcv)

rm(list = ls())

outpath <- "outMod/240222_Fr.rds"

### Load data ###

data.filename <- "data/otterDatCleaned.rds"

otterDat <- readRDS(data.filename) %>% 
  filter(code_insee %in% c("93", "27", "76", "84"))%>%
  st_as_sf(coords = geometry, crs = 2154)

### Keep only PACA ###

otterDat <- otterDat 

### Get gridded landscape ### 

grid.filename <- "data/L9310x10grid.rds"
# grid.filename <- "data/L9310x10grid_KDE.rds"

L93_grid.sp <- readRDS(grid.filename)%>%
  filter(code_insee %in% c("93", "27", "76", "84"))

L93_grid <- L93_grid.sp %>%
  st_drop_geometry()

npixel <- nrow(L93_grid)

L93_grid$intercept <- 1
L93_grid$logArea <- log(as.numeric(st_area(L93_grid.sp))/1000**2)

### Format data to run JAGS mod ### 

tmp.res <- 4 #years

otterDat$period = otterDat$year %/% tmp.res

pa.dat <- otterDat %>%
  st_drop_geometry() %>%
  filter(PNA.protocole, grid.cell %in% L93_grid$grid.cell) %>%
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
  filter(!PNA.protocole, as.logical(presence), grid.cell %in% L93_grid$grid.cell) %>%
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

### Set up the GAM ###

jags.file <- "JAGS/test.jags"

tmpDat <- L93_grid %>% 
  st_as_sf(coords = c("lon.l93", "lat.l93"), crs = 2154)%>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  data.frame(1, .)

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

out <- as.matrix(as.mcmc.list(mod), chains = T)

# saveRDS(out, outpath)

zzz <- out[, grep("z\\[", colnames(out))]
zz <- array(NA, dim = c(nrow(zzz), ncol(zzz)/nperiod, nperiod))
for(t in 1:nperiod){
  zz[,,t] <- zzz[1:nrow(zzz), ((t-1) * ncol(zzz) / nperiod + 1):(t*ncol(zzz)/nperiod)]
}
z <- apply(zz, c(2,3), mean)

lll <- out[, grep("lambda\\[", colnames(out))]
ll <- array(NA, dim = c(nrow(lll), ncol(lll)/nperiod, nperiod))
for(t in 1:nperiod){
  ll[,,t] <- lll[1:nrow(lll), ((t-1) * ncol(lll) / nperiod + 1):(t*ncol(lll)/nperiod)]
}
lam <- apply(ll, c(2,3), mean)

rm("zzz", "zz", "lll", "ll")

z.df <- data.frame(grid.cell = rep(L93_grid$grid.cell, nperiod),
                     meanz = c(z[, 1:nperiod]),
                     period = rep(unique(po.dat$period), each = nrow(z))) %>%
  left_join(L93_grid.sp, .)

z.df$z.occupied <- ifelse(z.df$meanz > 0.25, "OCCUPIED", "UNOCCUPIED")

otterDat.toplot <- otterDat%>%
  filter(PNA.protocole|as.logical(presence)) %>% 
  mutate(dataType = ifelse(PNA.protocole&as.logical(presence), "PNA presence",
                           ifelse(PNA.protocole, "PNA absence", "presence Opportuniste")))

ggplot()+
  geom_sf(data = z.df, aes(fill = z.occupied), alpha = 0.85) +
  geom_sf(data = otterDat.toplot, aes(color = dataType), alpha = 0.5, size = .6)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c('orange','white')) + 
  facet_wrap(~paste0(period*tmp.res, " - ", period*tmp.res+tmp.res-1))+
  theme_bw()

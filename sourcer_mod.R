library(tidyverse)
library(sf)
library(foreach)
library(mgcv)

library(rjags)
library(coda)

rm(list = ls())

source("src/functions/jags_ini_bis.R")

jagsPar <- list(N.CHAINS = 4,
                ADAPT = 500,
                BURNIN = 2000,
                SAMPLE = 1000,
                THIN = 1)

data.filename <- "data/otterDat.rds"
grid.filename <- "data/L9310x10grid_covs.rds"
effort.filename <- "data/samplingEffort.rds"
CF.filename <- "data/crayFishData.rds"

### Load data ------------------------------------------------------------------

otterDat_full <- readRDS(data.filename)

L93_grid_full <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

CFdata_full <- readRDS(CF.filename) %>%
  st_as_sf(crs = 2154)

effort_full <- readRDS(effort.filename)

### 1. Full country ------------------------------------------------------------

REGIONS <- c("72", "83", "25", "26", "53",
             "24", "43", "23", "91",
             "74", "73", "52",
             "54", "93", "82")

source("src/fit_JAGS_bis.R")

outpath <- paste0("out/","Mod_full_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat_full", "L93_grid_full", "effort_full", "my_inits", "jagsPar")))

### 2. North-West --------------------------------------------------------------

# REGIONS <- c("52", "53"," 24", "25")
# 
# source("src/fit_JAGS_bis.R")
# 
# outpath <- paste0("out/","Mod_NO_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")
# 
# save.image(file=outpath)

# rm(list = setdiff(ls(), c("otterDat_full", "L93_grid_full", "effort_full", "my_inits")))

### 3. South-East --------------------------------------------------------------

# REGIONS <- c("83", "91","73", "93", "82")
# 
# source("src/fit_JAGS_bis.R")
# 
# outpath <- paste0("out/","Mod_SE_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")
# 
# save.image(file=outpath)
# 
# rm(list = setdiff(ls(), c("otterDat_full", "L93_grid_full", "effort_full", "my_inits")))

### 4. East --------------------------------------------------------------

# REGIONS <- c("74", "83", "26", "24", "43", "82")
# 
# source("src/fit_JAGS_bis.R")
# 
# outpath <- paste0("out/","Mod_E_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")
# 
# save.image(file=outpath)
# 
# rm(list = setdiff(ls(), c("otterDat_full", "L93_grid_full", "effort_full", "my_inits")))
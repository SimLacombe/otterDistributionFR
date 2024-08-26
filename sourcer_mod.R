library(tidyverse)
library(sf)
library(foreach)
library(mgcv)

library(rjags)
library(coda)

rm(list = ls())

source("src/functions/jags_ini.R")

jagsPar <- list(N.CHAINS = 4,
                ADAPT = 1000,
                BURNIN = 10000,
                SAMPLE = 2500,
                THIN = 1,
                MONITOR = c(
                  "b",
                  "beta_latent",
                  "beta0_thin",
                  "rho_protocol",
                  "u_ent",
                  "sigma_ent",
                  "lambda_gam",
                  "tau_gam"
                ))

data.filename <- "data/otterDat.rds"
landscape.filename <- "data/landscape.rds"
effort.filename <- "data/samplingEffort.rds"
prey.filename <- "data/preyData.rds"

### Load data ------------------------------------------------------------------

otterDat <- readRDS(data.filename)

landscape_full <- readRDS(landscape.filename) %>%
  st_as_sf(crs = 2154)

preyData_full <- readRDS(prey.filename) %>%
  st_as_sf(crs = 2154)

effort_full <- readRDS(effort.filename)

### 1. Full country ------------------------------------------------------------

REGIONS <- c("72", "83", "25", "26", "53",
             "24", "43", "23", "91",
             "74", "73", "52", "22", "11", "31",
             "54", "93", "82", "42", "21", "41")

source("src/fit_JAGS.R")

outpath <- paste0("out/","Mod_full_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar")))

### 2. North-West --------------------------------------------------------------

REGIONS <- c("52", "53"," 24", "25")

source("src/fit_JAGS.R")

outpath <- paste0("out/","Mod_NO_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar")))

### 3. South-East --------------------------------------------------------------

REGIONS <- c("83", "91","73", "93", "82")

source("src/fit_JAGS.R")

outpath <- paste0("out/","Mod_SE_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar")))

# ### 4. East --------------------------------------------------------------

REGIONS <- c("74", "83", "26", "24", "43", "82")

source("src/fit_JAGS.R")

outpath <- paste0("out/","Mod_E_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".RData")

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar")))
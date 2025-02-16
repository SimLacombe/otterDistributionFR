library(tidyverse)
library(sf)
library(foreach)
library(mgcv)

library(rjags)
library(coda)

rm(list = ls())

source("src/functions/jags_ini.R")

jagsPar <- list(N.CHAINS = 4,
                ADAPT = 2000,
                BURNIN = 20000,
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

NSPLINES <- 15

### Load data ------------------------------------------------------------------

otterDat <- readRDS(data.filename)

landscape_full <- readRDS(landscape.filename) %>%
  st_as_sf(crs = 2154)

landscape_full$hydroLen <- c(scale(landscape_full$hydroLen))
landscape_full$ripProp <- c(scale(landscape_full$ripProp))

preyData_full <- readRDS(prey.filename) %>%
  st_as_sf(crs = 2154)

effort_full <- readRDS(effort.filename)

### 1. Full country ------------------------------------------------------------

randomEffect <- TRUE

REGIONS <- c("72", "83", "25", "26", "53",
             "24", "43", "23", "91",
             "74", "73", "52", "22", "11", "31",
             "54", "93", "82", "42", "21", "41")

source("src/fit_JAGS.R")

outpath <- "out/modFr.RData"

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar", "NSPLINES")))

### 2. North-West --------------------------------------------------------------

randomEffect <- FALSE

REGIONS <- c("52", "53","54", "24", "25")

source("src/fit_JAGS.R")

outpath <- "out/modNO.RData"

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar", "NSPLINES")))

### 3. South-East --------------------------------------------------------------

randomEffect <- FALSE

REGIONS <- c("83", "91","73", "93", "82")

source("src/fit_JAGS.R")

outpath <- "out/modSE.RData"

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar", "NSPLINES")))

# ### 4. East --------------------------------------------------------------------

randomEffect <- FALSE

REGIONS <- c("74", "83", "26", "24", "43", "82")

source("src/fit_JAGS.R")

outpath <- "out/modE.RData"

save.image(file=outpath)

rm(list = setdiff(ls(), c("otterDat", "landscape_full", "effort_full", "preyData_full", "my_inits", "jagsPar", "NSPLINES")))

# ### 5. Make figs ---------------------------------------------------------------
# 
# source("src/make_figs.R")
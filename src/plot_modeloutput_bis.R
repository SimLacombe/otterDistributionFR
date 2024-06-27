library(tidyverse)
library(sf)
library(LaplacesDemon)

rm(list = ls())

path <-"out/Mod_20240627_045116.RData"
  
load(path)

ISDM_dat <- L93_grid %>%
  select(px) %>%
  left_join(ISDM_dat, .) %>%
  st_as_sf(crs = 2154)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT OUTPUT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Occupied cells -------------------------------------------------------------

zz <- out[, grep("z\\[", colnames(out))]

ISDM_dat$estZ <- apply(zz, 2, mean)
ISDM_dat$sdZ <- apply(zz, 2, sd)

rm(zz)

### Psi ------------------------------------------------------------------------

bbb <- out[, grep("b\\[", colnames(out))]

ll <- map(seq_along(unique(ISDM_dat$t)), function(.t){
  ISDM_dat_t <- filter(ISDM_dat, t == .t)
  bbb[,(1:20) + (.t-1)*20] %*% t(gamDat$jags.data$X[ISDM_dat_t$px,]) + matrix(rep(ISDM_dat_t$logArea, nrow(out)),
                                                                              nrow = nrow(out),
                                                                              ncol = nrow(ISDM_dat_t),
                                                                              byrow = TRUE)
})

rm(bbb)

ll <- reduce(ll, cbind)

ISDM_dat$estLam <- apply(exp(ll), 2, mean)
ISDM_dat$sdLam <- apply(exp(ll), 2, sd)

rm(ll)

ISDM_dat$estPsi <- 1 - exp(-ISDM_dat$estLam)
ISDM_dat <- ISDM_dat %>% 
  group_by(px) %>%
  arrange(t) %>%
  mutate(dpsi = estPsi - lag(estPsi, 1))

### Beta_thin ------------------------------------------------------------------

bthin <- out[, grep("beta_thin", colnames(out))]

params <- data.frame(bthin = invlogit(bthin))

ggplot(params)+
  geom_violin(aes(x = 1, y = bthin))

### Plots ----------------------------------------------------------------------

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = estPsi), col = NA) +
  scale_fill_gradient2(
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint = .5
  ) +
  facet_wrap( ~ t) +
  theme_bw()

ggplot(ISDM_dat %>% filter(t>1)) +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(aes(fill = dpsi), col = NA, alpha = .85) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0
  ) +
  facet_wrap( ~ t) +
  theme_bw()

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = estZ), col = NA, alpha = .85) +
  scale_fill_gradient2(
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint = .5
  )  +
  facet_wrap( ~ t) +
  theme_bw()
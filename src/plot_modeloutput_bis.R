library(tidyverse)
library(sf)
library(LaplacesDemon)

rm(list = ls())

path <-"out/Mod_E_20240709_071954.RData"
  
load(path)

L93_grid <- L93_grid_full %>% 
  select(gridCell) %>%
  left_join(L93_grid, .) %>%
  st_as_sf(crs = 2154)

ISDM_dat <- L93_grid %>%
  select(px, hydroLen, ripArea) %>%
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

bbl <- out[, grep("beta_latent\\[", colnames(out))]

ll <- map(seq_along(unique(ISDM_dat$t)), function(.t){
  ISDM_dat_t <- ISDM_dat %>%
    st_drop_geometry %>% 
    filter(t == .t) 
  
  bbb[,(1:20) + (.t-1)*20] %*% t(gamDat$jags.data$X[ISDM_dat_t$px,]) +
    bbl %*% t(ISDM_dat_t[, c("hydroLen", "ripArea")]) +
    matrix(rep(ISDM_dat_t$logArea, nrow(out)),
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

latent_covs <- data.frame(alpha = exp(c(bbl[,1], bbl[,2])),
                          cov = rep(c("hydroLen", "ripArea"),each = 4000))

ggplot(latent_covs)+
  geom_violin(aes(x = cov, fill = cov, y = alpha))+
  theme_bw()

### Beta_thin ------------------------------------------------------------------

params <- out[, grep("beta_rho_protocol", colnames(out))] %>% 
  invlogit %>%
  data.frame

colnames(params) <- paste0("p", 1:ncol(params))

params <- pivot_longer(params, cols = starts_with("p"),
                       names_to = "protocol", values_to = "prob")%>%
  mutate(param = "rho")

params <- data.frame(prob = invlogit(out[, grep("beta_thin", colnames(out))]),
                     protocol = "b",
                    param = "b") %>%
  rbind(params, .)


ggplot(params)+
  geom_violin(aes(x = protocol, y = prob, fill = param)) +
  theme_bw()

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

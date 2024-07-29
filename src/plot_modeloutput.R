library(tidyverse)
library(sf)
library(LaplacesDemon)

rm(list = ls())

path <-"out/Mod_full_20240726_183027.RData"
  
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

# zz <- out[, grep("z\\[", colnames(out))]
# 
# ISDM_dat$estZ <- apply(zz, 2, mean)
# ISDM_dat$sdZ <- apply(zz, 2, sd)
# 
# rm(zz)

### Psi ------------------------------------------------------------------------

bbb <- out[, grep("b\\[", colnames(out))]

bbl <- out[, grep("beta_latent\\[", colnames(out))]

ll <- map(seq_along(unique(ISDM_dat$t)), function(.t){
  ISDM_dat_t <- ISDM_dat %>%
    st_drop_geometry %>% 
    filter(t == .t) 
  
  bbb[,(1:20) + (.t-1)*20] %*% t(gamDat$jags.data$X[ISDM_dat_t$px,]) +
    bbl %*% t(ISDM_dat_t[, c("hydroLen", "ripArea", "Crayfish")]) +
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

latent_covs <- data.frame(alpha = exp(c(bbl[,1], bbl[,2], bbl[,3])),
                          cov = rep(c("hydroLen", "ripArea", "Crayfish"),each = 4000))

ggplot(latent_covs)+
  geom_violin(aes(x = cov, fill = cov, y = alpha))+
  theme_bw()

### Beta_rho ------------------------------------------------------------------

beta_protocol <- out[, grep("beta_rho_protocol", colnames(out))] %>% 
  invlogit %>%
  data.frame

colnames(beta_protocol) <- paste0("p", 1:ncol(beta_protocol))

beta_protocol <- pivot_longer(beta_protocol, cols = starts_with("p"),
                       names_to = "protocol", values_to = "prob")

ggplot(beta_protocol)+
  geom_violin(aes(x = protocol, y = prob, fill = protocol)) +
  theme_bw()

### Beta_thin ------------------------------------------------------------------

entities <- c(levels(factor(effort)), "NA")

beta_ent <- out[, grep("beta_ent", colnames(out))] %>% 
  invlogit %>%
  apply(2, function(x){
    quantile(x, c(0.05,0.5,0.975))
    }) %>%
  t %>%
  data.frame

colnames(beta_ent) <- c("inf", "med", "sup")

beta_ent <- beta_ent %>%
  mutate(entity = rep(entities, 15),
         region = rep(1:length(entities), 15),
         t = rep(1:15, each = length(entities)),
         year = rep(2009:2023, each = length(entities))) %>%
  filter(entity != "NA")

ggplot(beta_ent)+
  geom_line(aes(x = year, y = med, col = entity), show.legend = F) +
  geom_ribbon(aes(x = year, ymin = inf, ymax = sup, col = entity, fill = entity),
              alpha = .5, show.legend = F) +
  facet_wrap(~entity) +
  theme_bw()

### Plots ----------------------------------------------------------------------

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = estPsi), col = NA) +
  scale_fill_gradient2(name = "psi",
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint = .5
  ) +
  facet_wrap( ~ (t+2008)) +
  theme_bw()

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = log(sdLam)), col = NA) +
  # scale_fill_gradient2(name = "psi",
  #                      low = "white",
  #                      mid = "orange",
  #                      high = "darkred",
  #                      midpoint = .5
  # ) +
  facet_wrap( ~ (t+2008)) +
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

library(tidyverse)
library(sf)
library(LaplacesDemon)

rm(list = ls())

path <-"out/Mod_full_20240803_232126.RData"
  
load(path)

L93_grid <- L93_grid_full %>% 
  select(gridCell, code_insee) %>%
  left_join(L93_grid, .) %>%
  st_as_sf(crs = 2154)

ISDM_dat <- L93_grid %>%
  select(px, code_insee) %>%
  left_join(ISDM_dat, .) %>%
  st_as_sf(crs = 2154)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT OUTPUT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Psi ------------------------------------------------------------------------

bbb <- out[, grep("b\\[", colnames(out))]

bbl <- out[, grep("beta_latent\\[", colnames(out))]

ll <- map(seq_along(unique(ISDM_dat$t)), function(.t){
  ISDM_dat_t <- ISDM_dat %>%
    st_drop_geometry %>% 
    filter(t == .t) 
  
  bbb[,(1:20) + (.t-1)*20] %*% t(gamDat$jags.data$X[ISDM_dat_t$px,]) +
    bbl %*% t(ISDM_dat_t[, c("hydroLen", "ripProp", "Crayfish", "Trout")]) +
    matrix(rep(ISDM_dat_t$logArea, nrow(out)),
           nrow = nrow(out),
           ncol = nrow(ISDM_dat_t),
           byrow = TRUE)
})

rm(bbb)

ll <- reduce(ll, cbind)

ISDM_dat$estLam <- apply(exp(ll), 2, mean)
ISDM_dat$estPsi <- apply(1 - exp(-exp(ll)), 2, mean)
ISDM_dat$sdPsi <- apply(1 - exp(-exp(ll)), 2, sd)

rm(ll)

ISDM_dat <- ISDM_dat %>% 
  group_by(px) %>%
  arrange(t) %>%
  mutate(dpsi = estPsi - lag(estPsi, 1))

### Beta_lam -------------------------------------------------------------------

simRip <- seq(min(ISDM_dat$ripProp, na.rm = T),
              max(ISDM_dat$ripProp, na.rm = T),
              length.out = 100)

simHydr <- seq(min(ISDM_dat$hydroLen, na.rm = T),
              max(ISDM_dat$hydroLen, na.rm = T),
              length.out = 100)

pred <- matrix(0, nrow = 200, ncol = 4)

pred[1:100, 1] <- simHydr
pred[101:200, 2] <- simRip

Lam0 <- out[, grep("b\\[1,", colnames(out))] %>% 
  apply(1, mean)

simLam <- (Lam0 + bbl %*% t(pred) + log(100)) %>%
  exp 

simPsi <- 1 - exp(-simLam) %>%
  apply(2, function(x){quantile(x, c(0.025,0.5,0.975))})

latent_continuous_cov <- data.frame(inf = simPsi[1,],
                                    med = simPsi[2,],
                                    sup = simPsi[3,],
                                    x = c(simHydr, simRip),
                                    cov = rep(c("River len.", "Riparian prop."), each = 100))

pred_d <- matrix(0, 4, 4)
pred_d[2,3] <- 1 
pred_d[4,4] <- 1 

simLam_d <- (Lam0 + bbl %*% t(pred_d) + log(100)) %>%
  exp 
simPsi_d <- 1 - exp(-simLam_d) %>%
  apply(2, function(x){quantile(x, c(0.025,0.5,0.975))})

latent_discrete_cov <- data.frame(inf = simPsi_d[1,],
                                  med = simPsi_d[2,],
                                  sup = simPsi_d[3,],
                                  x = c(0, 1, 0, 1),
                                  cov = rep(c("Crayfish", "Trout"), each = 2))

ggplot(latent_continuous_cov)+
  geom_line(aes(x = x, col = cov, y = med), show.legend = F)+
  geom_ribbon(aes(x = x, col = cov, fill = cov, ymin = inf, ymax = sup), alpha = 0.5, show.legend = F) +
  geom_errorbar(data = latent_discrete_cov, aes(x = x, col = cov, ymin = inf, ymax = sup),
                width = 0.125, show.legend = F)+
  geom_point(data = latent_discrete_cov, aes(x = x, col = cov, y = med), show.legend = F)+
  facet_wrap(~cov, scale = "free_x") +
  xlab("") + 
  ylab("") +
  theme_bw()

### Beta_rho ------------------------------------------------------------------

rho0 <- out[, grep("beta0_rho", colnames(out))]

u_protocol <- out[, grep("u_protocol", colnames(out))] %>% 
  data.frame

colnames(u_protocol) <- c("pointwise", "transect")

u_protocol <- pivot_longer(u_protocol, cols = c("pointwise", "transect"),
                       names_to = "protocol", values_to = "u") %>%
  mutate(rho = invlogit(u + rep(rho0, each = 2)))

ggplot(u_protocol)+
  geom_violin(aes(x = protocol, y = rho, fill = protocol)) +
  theme_bw()

### Beta_thin ------------------------------------------------------------------

b0 <- out[, grep("beta0_thin", colnames(out))]

u_ent <- out[, grep("u_ent", colnames(out))]

p_ent <- invlogit(u_ent + matrix(rep(b0, ncol(u_ent)), ncol = ncol(u_ent)))

p_ent <- p_ent %>% 
  apply(2, function(x){
    quantile(x, c(0.05,0.5,0.975))
    }) %>%
  t %>%
  data.frame

colnames(p_ent) <- c("inf", "med", "sup")

p_ent <- ISDM_dat %>%
  st_drop_geometry %>% 
  group_by(t, ent, ent.year) %>% 
  summarize() %>%
  mutate(year = t + 2008) %>%
  arrange(ent.year) %>%
  cbind(p_ent) %>%
  filter(ent != "0")

ggplot(p_ent)+
  geom_line(aes(x = year, y = med, col = ent), show.legend = F) +
  geom_ribbon(aes(x = year, ymin = inf, ymax = sup, col = ent, fill = ent),
              alpha = .5, show.legend = F) +
  geom_hline(aes(yintercept = 0.5), color = "red", linetype = "dashed") + 
  facet_wrap(~ent, scale = "free_y") +
  xlab("") + ylab("") +
  theme_bw()

### Plots ----------------------------------------------------------------------

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = estPsi > 0.5), col = NA) +
  # scale_fill_gradient2(name = "psi",
  #   low = "white",
  #   mid = "orange",
  #   high = "darkred",
  #   midpoint = .5
  # ) +
  facet_wrap( ~ (t+2008)) +
  theme_bw()

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = sdPsi), col = NA) +
  scale_fill_viridis_c() +
  facet_wrap( ~ (t+2008)) +
  theme_bw()

ggplot() +
  geom_sf(data = L93_grid, fill = "lightgrey", col = NA) +
  geom_sf(data = ISDM_dat, aes(fill = dpsi), col = NA) +
  scale_fill_gradient2(name = "delta psi",
                       low = "red",
                       mid = "white",
                       high = "green",
                       midpoint = 0
  ) +
  facet_wrap( ~ (t+2008)) +
  theme_bw()

### Proportion of occupied cells -----------------------------------------------

summPsi <- ISDM_dat %>% 
  st_drop_geometry() %>% 
  group_by(t) %>% 
  summarize(f.occ = mean(estPsi),
            sd.occ = sd(estPsi))

ggplot(summPsi) +
  geom_line(aes(x = t, y = f.occ)) +
  geom_ribbon(aes(x = t, ymin = f.occ - 1.96 * sd.occ, ymax = f.occ + 1.96 * sd.occ),
              alpha = 0.5, col = "lightblue", fill = "lightblue")+
  theme_bw()
            
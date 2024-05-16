library(tidyverse)

path <-"outMod/2024-05-16_ISDM_Br.PdL.No_4yrs.rds"
  paste0("outMod/",
         paste(Sys.Date(), MOD, paste(REGIONS, collapse = "."), TIMEPERIOD,
               sep = "_"),
         "yrs.rds")
  
out <- readRDS(path)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT OUTPUT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Occupied cells -------------------------------------------------------------

zzz <- out[, grep("z\\[", colnames(out))]
zz <- array(NA, dim = c(nrow(zzz), ncol(zzz) / nperiod, nperiod))
for (t in 1:nperiod) {
  zz[, , t] <-
    zzz[1:nrow(zzz), ((t - 1) * ncol(zzz) / nperiod + 1):(t * ncol(zzz) / nperiod)]
}
z <- apply(zz, c(2, 3), mean)

rm("zzz", "zz")

z.df <- data.frame(
  gridCell = rep(L93_grid$gridCell, nperiod),
  meanz = c(z[, 1:nperiod]),
  period = rep(unique(poDat$period), each = nrow(z))
) %>%
  left_join(L93_grid, .) %>%
  mutate(period = paste0(period * TIMEPERIOD, " - ", period * TIMEPERIOD + TIMEPERIOD -
                           1))

z.df$z.occupied <-
  ifelse(z.df$meanz > 0.25, "OCCUPIED", "UNOCCUPIED")

### Psi ------------------------------------------------------------------------

bbb <- out[, grep("b\\[", colnames(out))]
ll <- array(NA, dim = c(nrow(bbb), nrow(L93_grid), nperiod))

for (i in 1:nrow(ll)) {
  ll[i, , ] <-
    gamDat$jags.data$X %*% matrix(bbb[i, ], nrow = NSPLINES, ncol = nperiod) + matrix(rep(L93_grid$logArea, nperiod),
                                                                                      nrow = nrow(L93_grid),
                                                                                      ncol = nperiod)
}

l <- exp(apply(ll, c(2, 3), mean))

sum.l <-
  exp(apply(
    ll,
    3,
    FUN = function(x) {
      quantile(x, c(0.25, 0.5, 0.75))
    }
  ))

rm("bbb", "ll")

latent.df <-
  data.frame(
    gridCell = rep(L93_grid$gridCell, nperiod),
    psi = 1 - exp(-c(l)),
    period = rep(unique(poDat$period), each = nrow(l))
  ) %>%
  left_join(L93_grid, .) %>%
  mutate(period = paste0(period * TIMEPERIOD, " - ", period * TIMEPERIOD + TIMEPERIOD -
                           1))

latent.df.peryear <- data.frame(
  period = unique(poDat$period),
  psi.inf = 1 - exp(-sum.l[1, ]),
  psi.med = 1 - exp(-sum.l[2, ]),
  psi.sup = 1 - exp(-sum.l[3, ])
) %>%
  mutate(period = paste0(period * TIMEPERIOD, " - ", period * TIMEPERIOD + TIMEPERIOD -
                           1))

### Region x year --------------------------------------------------------------

beta_reg <- out[, grep("beta_region\\[", colnames(out))]
sum.beta_reg <-
  apply(
    beta_reg,
    2,
    FUN = function(x) {
      quantile(x, c(0.25, 0.5, 0.75))
    }
  )
beta_reg.df <-
  data.frame(
    period = rep(unique(poDat$period), each = length(unique(REGIONS))),
    region = rep(unique(REGIONS), nperiod),
    beta_reg.inf = sum.beta_reg[1, ],
    beta_reg.med = sum.beta_reg[2, ],
    beta_reg.sup = sum.beta_reg[3, ]
  ) %>%
  mutate(period = paste0(period * TIMEPERIOD, " - ", period * TIMEPERIOD + TIMEPERIOD -
                           1))

### Plots ----------------------------------------------------------------------

otterDat.toplot <- otterDat %>%
  mutate(
    dataType = ifelse(
      protocol != "PO" & as.logical(presence),
      "PA - presence",
      ifelse(protocol != "PO", "PA - absence", "PO")
    ),
    period = paste0(period * TIMEPERIOD, " - ", period * TIMEPERIOD + TIMEPERIOD -
                      1)
  ) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 2154)

ggplot() +
  geom_sf(data = latent.df, aes(fill = psi), alpha = .85) +
  geom_sf(data = otterDat.toplot,
          aes(color = dataType),
          alpha = .5,
          size = .6) +
  scale_color_manual(values = c("red", "blue", "black")) +
  scale_fill_gradient2(
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint = .5
  ) +
  facet_wrap( ~ period) +
  theme_bw()

ggplot() +
  geom_sf(data = z.df, aes(fill = z.occupied), alpha = .85) +
  geom_sf(data = otterDat.toplot,
          aes(color = dataType),
          alpha = .5,
          size = .6) +
  scale_color_manual(values = c("red", "blue", "black")) +
  scale_fill_manual(values = c("orange", "white")) +
  facet_wrap( ~ period) +
  theme_bw()

ggplot(latent.df.peryear) +
  geom_line(aes(x = as.numeric(as.factor(period)), y = psi.med)) +
  geom_ribbon(aes(x = as.numeric(as.factor(period)), ymin = psi.inf, ymax = psi.sup),
              alpha = 0.5,
              fill = "transparent",
              col = "black") +
  theme_bw()

ggplot(beta_reg.df) +
  geom_pointrange(
    aes(
      x = region,
      y = beta_reg.med,
      ymin = beta_reg.inf,
      ymax = beta_reg.sup,
      color = region
    ),
    show.legend = F
  ) +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  facet_wrap( ~ period)


ggplot(rho.df) +
  geom_violin(aes(
    x = protocole,
    y = rho,
    fill = protocole,
    group = protocole
  ),
  show.legend = F) +
  geom_hline(aes(yintercept = 0)) +
  theme_bw()

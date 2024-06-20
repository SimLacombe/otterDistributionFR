library(tidyverse)
library(sf)
library(mgcv)

rm(list = ls())

path <-"out/2024-06-19_72.83.25.26.53.24.43.23.91.74.73.52.54.93.82_1yrs.rds"
  
out <- readRDS(path)

# Crop Paris + NE
REGIONS <- c("72", "83", "25", "26", "53",
             "24", "43", "23", "91",
             "74", "73", "52",
             "54", "93", "82")

TIMEPERIOD <- 1 #years

TIMELAG <- TIMEPERIOD - 2009 %% TIMEPERIOD

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA AND COVS ~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Load data ------------------------------------------------------------------

data.filename <- "data/otterDat.rds"
grid.filename <- "data/L9310x10grid.rds"
map.filename <- "data/map_fr.rds"
effort.filename <- "data/samplingEffort.rds"

otterDat <- readRDS(data.filename)

L93_grid <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

map <- readRDS(map.filename) %>%
  st_as_sf(crs = 2154)

effort <- readRDS(effort.filename)

### Filter the region of interest ----------------------------------------------

effort <- effort[L93_grid$code_insee %in% REGIONS, ]

otterDat <- filter(otterDat, code_insee %in% REGIONS)
L93_grid <- filter(L93_grid, code_insee %in% REGIONS)

### Get offset and spatial covariates ------------------------------------------

L93_grid$intercept <- 1
L93_grid$logArea <- log(as.numeric(st_area(L93_grid)) / 1000 ** 2)

### Get primary period ---------------------------------------------------------

otterDat$period <- (otterDat$year + TIMELAG) %/% TIMEPERIOD

nperiod <- length(unique(otterDat$period))

### GAM Data -------------------------------------------------------------------

NSPLINES = 20

jags.file <- "src/JAGS/test.jags"

tmpDat <- L93_grid %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  data.frame(1, .)

names(tmpDat) <- c("y", "E", "N")

gamDat <- jagam(
  y ~ s(
    E,
    N,
    k = NSPLINES,
    bs = "ds",
    m = c(1, 0.5)
  ),
  data = tmpDat,
  file = jags.file,
  family = "binomial"
)

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT OUTPUT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Occupied cells -------------------------------------------------------------

zzz <- out[, grep("z\\[", colnames(out))]
zz <- array(NA, dim = c(nrow(zzz), ncol(zzz) / nperiod, nperiod))
for (t in 1:nperiod) {
  zz[, , t] <-
    zzz[1:nrow(zzz), ((t - 1) * ncol(zzz) / nperiod + 1):(t * ncol(zzz) / nperiod)]
}

z <- apply(zz, c(2, 3), mean)
sz <- apply(zz, c(2,3), sd)

rm("zzz", "zz")

z.df <- data.frame(
  gridCell = rep(L93_grid$gridCell, nperiod),
  meanz = c(z[, 1:nperiod]),
  sz = c(sz[, 1:nperiod]),
  period = rep(unique(otterDat$period), each = nrow(z))
) %>%
  left_join(L93_grid, .) %>%
  mutate(period = paste0(period * TIMEPERIOD - TIMELAG, " - ", period * TIMEPERIOD - TIMELAG + TIMEPERIOD -
                           1))

z.df$z.occupied <-
  ifelse(z.df$meanz > 0.50, "OCCUPIED", "UNOCCUPIED")

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
    period = rep(sort(unique(otterDat$period)), each = nrow(l))
  ) %>%
  left_join(L93_grid, .) %>%
  mutate(period = paste0(period * TIMEPERIOD - TIMELAG, " - ", period * TIMEPERIOD - TIMELAG + TIMEPERIOD -
                           1)) %>% 
  group_by(gridCell) %>% 
  mutate(dpsi = psi-lag(psi)) %>%
  ungroup()

latent.df.peryear <- data.frame(
  period = sort(unique(otterDat$period)),
  psi.inf = 1 - exp(-sum.l[1, ]),
  psi.med = 1 - exp(-sum.l[2, ]),
  psi.sup = 1 - exp(-sum.l[3, ])
) %>%
  mutate(period = paste0(period * TIMEPERIOD - TIMELAG, " - ", period * TIMEPERIOD - TIMELAG + TIMEPERIOD -
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
    period = rep(sort(unique(otterDat$period)), each = length(unique(REGIONS))),
    region = rep(unique(REGIONS), nperiod),
    beta_reg.inf = sum.beta_reg[1, ],
    beta_reg.med = sum.beta_reg[2, ],
    beta_reg.sup = sum.beta_reg[3, ]
  ) %>%
  mutate(period = paste0(period * TIMEPERIOD - TIMELAG, " - ", period * TIMEPERIOD - TIMELAG + TIMEPERIOD -
                           1))

### Plots ----------------------------------------------------------------------

ggplot() +
  geom_sf(data = latent.df, aes(fill = psi), col = NA, alpha = .85) +
  geom_sf(data = map, fill = "transparent")+
  scale_fill_gradient2(
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint = .5
  ) +
  facet_wrap( ~ period) +
  theme_bw()

ggplot() +
  geom_sf(data = latent.df, aes(fill = dpsi), col = NA, alpha = .85) +
  geom_sf(data = map, fill = "transparent")+
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0
  ) +
  facet_wrap( ~ period) +
  theme_bw()

ggplot() +
  geom_sf(data = z.df, aes(fill = meanz), col = NA, alpha = .85) +
  scale_fill_gradient2(
    low = "white",
    mid = "orange",
    high = "darkred",
    midpoint = .5
  )  +
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

eff.df <- cbind(L93_grid, effort)
colnames(eff.df)[7:21] <- paste0("yr", 1:15)
eff.df <- pivot_longer(eff.df, cols = all_of(paste0("yr", 1:15)),
                       names_to = "year", values_to = "effort") %>%
  mutate(year = factor(year, levels = paste0("yr", 1:15)))

ggplot(eff.df) +
  geom_sf(aes(fill = effort)) +
  facet_wrap(~year)

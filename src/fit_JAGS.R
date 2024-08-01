### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA AND COVS ~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Adapt effort matrix to sp and tmp resolution -------------------------------

effort <- effort_full[L93_grid_full$code_insee %in% REGIONS, ]
colnames(effort) <- paste0("yr.", 2009:2023)

### Filter the region of interest ----------------------------------------------

otterDat <- filter(otterDat_full, code_insee %in% REGIONS)
L93_grid <- filter(L93_grid_full, code_insee %in% REGIONS)
CFdata <- filter(CFdata_full, gridCell %in% L93_grid$gridCell) %>%
  mutate(year = paste0("yr.", year)) %>% 
  st_drop_geometry %>%
  select(-data.avail)

### Get offset and spatial covariates ------------------------------------------

L93_grid$logArea <- log(as.numeric(st_area(L93_grid)) / 1000 ** 2)
L93_grid$hydroLen <- c(scale(L93_grid$hydroLen))
L93_grid$ripArea <- c(scale(L93_grid$ripArea))

### Get pixel identifiers ------------------------------------------------------

L93_grid$px <- 1:nrow(L93_grid)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~ FORMAT DATA FOR JAGS ~~~~~~~~~~~~~~~~~~~~~~~~ ###

ISDM_dat <- cbind(st_drop_geometry(L93_grid), effort) %>%
  pivot_longer(cols = all_of(paste0("yr.", 2009:2023)),
               names_to = "year", values_to = "ent") %>%
  mutate(is_po_sampled = as.numeric(!is.na(ent))) %>% 
  arrange(year, px)

### add CF data ----------------------------------------------------------------

ISDM_dat <- ISDM_dat %>%
  left_join(CFdata, by = c("year", "gridCell")) %>% 
  mutate(Crayfish = as.numeric(Crayfish >= 0.75))

### Presence-Absence data ------------------------------------------------------

ISDM_dat <- otterDat %>%
  filter(protocol != "PO", gridCell %in% L93_grid$gridCell) %>%
  group_by(year, gridCell, protocol) %>%
  summarize(
    K = n(),
    ypa = sum(presence)
  ) %>%
  mutate(year = paste0("yr.", year)) %>%
  left_join(ISDM_dat, .)

### Presence-Only data ---------------------------------------------------------

ISDM_dat <- otterDat %>%
  filter(protocol  == "PO",
         gridCell %in% L93_grid$gridCell) %>%
  group_by(year, gridCell) %>%
  summarize(ypo = n()) %>%
  mutate(year = paste0("yr.", year)) %>%
  left_join(ISDM_dat, .)

### remove all NAs -------------------------------------------------------------

ISDM_dat <- modify(ISDM_dat, ~ ifelse(is.na(.x), 0, .x))

### arrange --------------------------------------------------------------------

ISDM_dat <- ISDM_dat %>%
  filter(is_po_sampled|sign(K)) %>% 
  mutate(t = as.numeric(as.factor(year)),
         protocol.fact = as.numeric(as.factor(protocol)),
         ent.year = as.numeric(as.factor(paste0(t, ent)))) %>%
  select(px, t, ent, ent.year, is_po_sampled, K, ypa, protocol,
         protocol.fact, ypo, logArea, hydroLen, ripArea, Crayfish)

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

rm(tmpDat)

gamDat$jags.ini$b[1] <- -4.6 #log area of cells

### Format data list -----------------------------------------------------------
L93_grid <- st_drop_geometry(L93_grid)

data.list <- list(
  npxt = nrow(ISDM_dat),
  pxts_pa = which(ISDM_dat$K>0),
  pxts_po = which(ISDM_dat$is_po_sampled==1),
  nyear = length(unique(ISDM_dat$t)),
  nent = max(ISDM_dat$ent.year),
  px = ISDM_dat$px,
  t = ISDM_dat$t,
  t2 = ISDM_dat$t2,
  ent.yr = ISDM_dat$ent.year,
  ypa = ISDM_dat$ypa,
  K = ISDM_dat$K,
  ypo = ISDM_dat$ypo,
  nprotocols = length(unique(ISDM_dat$protocol)) - 1,
  pa_protocol = ISDM_dat$protocol.fact - 1,
  ncov_lam = 3,
  cell_area = ISDM_dat$logArea,
  x_latent =  as.matrix(ISDM_dat[, c("hydroLen", "ripArea", "Crayfish")]),
  nspline = length(gamDat$jags.data$zero),
  x_gam = gamDat$jags.data$X,
  S1 = gamDat$jags.data$S1,
  zero = gamDat$jags.data$zero
)

inits <- foreach(i = 1:4) %do% {
  my_inits(i)
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ RUN MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Call jags ------------------------------------------------------------------

mod <- jags.model(
  file = "src/JAGS/JAGSmod.R",
  data = data.list,
  inits = inits,
  n.chains = jagsPar$N.CHAINS,
  n.adapt = jagsPar$ADAPT)

update(mod, jagsPar$BURNIN)

mcmc <- coda.samples(
  mod,
  variable.names = jagsPar$MONITOR,
  n.iter = jagsPar$SAMPLE,
  thin = jagsPar$THIN
)

rm(mod, jagsPar, inits)

out <- as.matrix(as.mcmc.list(mcmc), chains = T)
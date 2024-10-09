### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GET DATA AND COVS ~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### Adapt effort matrix to sp and tmp resolution -------------------------------

effort <- effort_full[landscape_full$code_insee %in% REGIONS, ]
colnames(effort) <- paste0("yr.", 2009:2023)

### Filter the region of interest ----------------------------------------------

landscape <- filter(landscape_full, code_insee %in% REGIONS)
preyData <- filter(preyData_full, gridCell %in% landscape$gridCell) %>%
  mutate(year = paste0("yr.", year)) %>% 
  st_drop_geometry

### Get offset and spatial covariates ------------------------------------------

landscape$logArea <- log(as.numeric(st_area(landscape)) / 1000 ** 2)

### Get pixel identifiers ------------------------------------------------------

landscape$px <- 1:nrow(landscape)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~ FORMAT DATA FOR JAGS ~~~~~~~~~~~~~~~~~~~~~~~~ ###

ISDM_dat <- cbind(st_drop_geometry(landscape), effort) %>%
  pivot_longer(cols = all_of(paste0("yr.", 2009:2023)),
               names_to = "year", values_to = "ent") %>%
  mutate(is_po_sampled = as.numeric(!is.na(ent))) %>% 
  arrange(year, px)

### add prey data --------------------------------------------------------------

ISDM_dat <- ISDM_dat %>%
  left_join(preyData, by = c("year", "gridCell")) %>% 
  mutate(Crayfish = as.numeric(Crayfish >= 0.75),
         Trout = as.numeric(Trout >= 0.75))

### Presence-Absence data ------------------------------------------------------

ISDM_dat <- otterDat %>%
  filter(protocol != "PO", gridCell %in% landscape$gridCell) %>%
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
         gridCell %in% landscape$gridCell) %>%
  group_by(year, gridCell) %>%
  summarize(ypo = n()) %>%
  mutate(year = paste0("yr.", year)) %>%
  left_join(ISDM_dat, .)

### remove all NAs -------------------------------------------------------------

ISDM_dat <- modify(ISDM_dat, ~ ifelse(is.na(.x), 0, .x))

### arrange --------------------------------------------------------------------

ISDM_dat <- ISDM_dat %>%
  filter(is_po_sampled|K>0) %>% 
  mutate(t = as.numeric(as.factor(year)),
         protocol.fact = as.numeric(as.factor(protocol)),
         ent.year = as.numeric(as.factor(paste0(t, ent)))) %>%
  select(px, t, ent, ent.year, is_po_sampled, K, ypa, protocol,
         protocol.fact, ypo, logArea, hydroLen, ripProp, Crayfish, Trout)

### GAM Data -------------------------------------------------------------------

jags.file <- "src/JAGS/test.jags"

tmpDat <- landscape %>%
  filter(px %in% ISDM_dat$px) %>% 
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

ISDM_dat <- ISDM_dat %>%
  mutate(idx_gam = as.numeric(as.factor(px)))

### Format data list -----------------------------------------------------------
landscape <- st_drop_geometry(landscape)

if(!randomEffect) {ISDM_dat$ent.year <- 1}

data.list <- list(
  npxt = nrow(ISDM_dat),
  pxts_pa = which(ISDM_dat$K>0),
  pxts_po = which(ISDM_dat$is_po_sampled==1),
  nyear = length(unique(ISDM_dat$t)),
  nent = max(ISDM_dat$ent.year),
  px = ISDM_dat$idx_gam,
  t = ISDM_dat$t,
  ent.yr = ISDM_dat$ent.year,
  ypa = ISDM_dat$ypa,
  K = ISDM_dat$K,
  ypo = ISDM_dat$ypo,
  nprotocols = length(unique(ISDM_dat$protocol)) - 1,
  pa_protocol = ISDM_dat$protocol.fact - 1,
  ncov_lam = 4,
  cell_area = ISDM_dat$logArea,
  x_latent =  as.matrix(ISDM_dat[, c("hydroLen", "ripProp", "Crayfish", "Trout")]),
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

out <- as.matrix(as.mcmc.list(mcmc), chains = T)
library(tidyverse)
library(sf)
library(purrr)
library(ggpubr)
library(rjags)

rm(list = ls())

source("src/functions/plotting_functions.R")

years_toplot <- c(2009, 2012, 2015, 2018, 2021, 2023)

allOut <- new.env()

### LOAD MODEL OUTPUTS ---------------------------------------------------------

paths <- list(Fr = "out/modFr.RData",
              NO = "out/ModNO.RData",
              E = "out/ModE.RData",
              SE = "out/ModSE.RData")

envs <- map(paths, get_model)

outs <- map(envs, 1)
dats <- map(envs, 2)
grids <- map(envs, 3)
XGAMs <- map(envs, 4)

### Check convergence ----------------------------------------------------------

params_toplot <- c("beta_latent[1]","beta_latent[2]","beta_latent[3]","beta_latent[4]",
                   "beta0_thin", "rho_protocol[1]", "rho_protocol[2]",
                   "lambda_gam", "tau_gam", "sigma_ent",
                   "b[1,1]", "b[10,13]", "b[2,12]", "b[2,6]")
  
allOut$diags <- map(outs, function(out){
  gelman.diag(as.mcmc.list(out))
})

allOut$posteriors <- map(outs, function(out){
  out[, params_toplot] %>% 
    as.matrix(chains = TRUE) %>% 
    as.data.frame %>% 
    pivot_longer(cols = all_of(params_toplot)) %>% 
    ggplot()+
    geom_density(aes(x = value, col = factor(CHAIN)))+
    geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
    facet_wrap(~name, scales = "free")+
    theme_bw()
})

### MAPS FOR THE WHOLE COUNTRY -------------------------------------------------

outs <- map(outs, as.matrix)

gridFull <- readRDS("data/landscape.rds")

dats <- pmap(list(dats, outs, XGAMs), get_psi)
  
grids <- map(grids, add_geom, gridFull)

sp_dats <- map2(dats, grids, add_geom2)

dats_toplot <- map(sp_dats, function(x){x %>% filter((t + 2008) %in% years_toplot)})

map_occ <- plot_map(dats_toplot[[1]], grids[[1]], estPsi > 0.5, show.legend = FALSE)+
  facet_wrap(~ (t + 2008), ncol = 3) + 
  scale_fill_manual(values = c("white", "chartreuse4"))

map_sd <- plot_map(dats_toplot[[1]], grids[[1]], sdPsi, show.legend = TRUE)+
  facet_wrap(~ (t + 2008), ncol = 3) + 
  scale_fill_viridis_c(name = "", option = "plasma", breaks = c(0, 0.125, 0.25))

allOut$fig1 <- ggarrange(map_occ, map_sd, nrow = 2, common.legend = T, legend = "bottom")

### MAPS FOR LOCAL MODELS ------------------------------------------------------

plot.list <- pmap(list(dats_toplot[c(2,3,4)], grids[c(2,3,4)], c("plum2", "lightblue", "lightgreen")), function(dat, grid, col){
  plot_map(dat, grid, estPsi > 0.5, show.legend = FALSE) +
    geom_sf(data = st_union(grid), fill = NA) +
    facet_wrap(~ (t + 2008), ncol = 3) + 
    scale_fill_manual(values = c("white", col))
})

helper <- ggplot()+
  geom_sf(data = st_union(grids[[1]])) +
  geom_sf(data = st_union(grids[[2]]), col = "plum2", fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[3]]), col = "lightblue", fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[4]]), col = "lightgreen", fill = NA, linewidth = 2) +
  theme_void()

allOut$fig2 <- ggarrange(helper, plot.list[[3]], plot.list[[1]], plot.list[[2]],
          nrow = 2, ncol = 2, align = "hv")

### AVERAGE OCCUPANCY ----------------------------------------------------------

avg_occ <- pmap(list(dats, outs, XGAMs), get_avg_occ)
avg_occ <- imap(avg_occ, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind) %>%
  mutate(model = factor(model, levels = names(envs)))

allOut$fig3 <- ggplot(avg_occ) +
  geom_ribbon(aes(x = t, ymin = inf, ymax = sup, fill = model), alpha = 0.5)+
  geom_line(aes(x = t, y = med, col = model), linewidth = 1.25) + 
  xlab("time")+ ylab("Proportion of occupied cells")+
  scale_fill_manual(values = c("darkgrey", "plum2", "lightblue", "lightgreen"))+
  scale_color_manual(values = c("darkgrey", "plum2", "lightblue", "lightgreen"))+
  theme_bw()

### COVARIATE EFFECTS ----------------------------------------------------------

pred_c <- map2(outs, grids, predict_c, off = log(100))
pred_c <- imap(pred_c, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind) %>%
  mutate(model = factor(model, levels = names(envs)))

pred_d <- map(outs, predict_d, off = log(100))
pred_d <- imap(pred_d, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind) %>%
  mutate(model = factor(model, levels = names(envs)))

plot_c <- ggplot(pred_c) +
  geom_line(aes(x=x, y=med, col = model)) +
  geom_ribbon(aes(x = x, col = model, fill = model,
                  ymin = inf, ymax = sup), alpha = 0.5) +
  scale_fill_manual(values = c("darkgrey", "plum2", "lightblue", "lightgreen"))+
  scale_color_manual(values = c("darkgrey", "plum2", "lightblue", "lightgreen"))+
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("") +
  theme_bw()

plot_d <- ggplot(pred_d) +
  geom_violin(aes(x=x, y=prob, fill = model), show.legend = F) +
  scale_fill_manual(values = c("darkgrey", "plum2", "lightblue", "lightgreen"))+
  scale_color_manual(values = c("darkgrey", "plum2", "lightblue", "lightgreen"))+
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("") +
  theme_bw()

allOut$fig4 <- ggarrange(plot_d, plot_c, nrow = 2, common.legend = T, legend = "right")

### THINNING PROBABILITY -------------------------------------------------------

beta0_thin <- outs$Fr[, grepl("beta0_thin", colnames(outs$Fr)), drop = FALSE]

u_out <- outs$Fr[, grepl("u_ent", colnames(outs$Fr))] %>% 
  apply(2, function(x) quantile(invlogit(x + beta0_thin), probs = c(0.025, 0.5, 0.975))) %>%
  t

colnames(u_out) <- c("inf", "med", "sup")

thin_prob <- dats$Fr %>% 
  group_by(t, ent, ent.year) %>%
  summarize() %>%
  cbind(u_out) %>%
  filter(!ent == 0)

allOut$fig5 <- ggplot(thin_prob)+
  geom_ribbon(aes(x = t + 2008, ymin = inf, ymax = sup, fill = ent), alpha = 0.5, show.legend = F)+
  geom_line(aes(x = t + 2008, y = med, col = ent), show.legend = F)+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~ent)

save(allOut, file = "out/plots.RData")

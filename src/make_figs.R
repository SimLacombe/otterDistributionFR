library(tidyverse)
library(sf)
library(LaplacesDemon)
library(purrr)
library(ggpubr)

rm(list = ls())

source("src/functions/plotting_functions.R")

years_toplot <- c(2009, 2012, 2015, 2018, 2021, 2023)

gradPsi <- scale_fill_gradient2(name = "psi",
                     low = "white",
                     mid = "orange",
                     high = "darkred",
                     midpoint = .5)

### LOAD MODEL OUTPUTS ---------------------------------------------------------

paths <- list(Fr = "out/Mod_full_20240803_232126.RData",
              NO = "out/Mod_NO_20240806_213836.RData",
              E = "out/Mod_E_20240808_202739.RData",
              SE = "out/Mod_SE_20240807_030954.RData")

envs <- map(paths, get_model)

outs <- map(envs, 1)
dats <- map(envs, 2)
grids <- map(envs, 3)
XGAMs <- map(envs, 4)

### MAPS FOR THE WHOLE COUNTRY -------------------------------------------------

gridFull <- readRDS("data/landscape.rds")

dats <- pmap(list(dats, outs, XGAMs), get_psi)
  
grids <- map(grids, add_geom, gridFull)

dats <- map2(dats, grids, add_geom2)

dats_toplot <- map(dats, function(x){x %>% filter((t + 2008) %in% years_toplot)})

map_occ <- plot_map(dats_toplot[[1]], grids[[1]], estPsi > 0.5, show.legend = FALSE)+
  facet_wrap(~ (t + 2008), ncol = 3) + 
  scale_fill_manual(values = c("white", "chartreuse4"))

map_sd <- plot_map(dats_toplot[[1]], grids[[1]], sdPsi, show.legend = TRUE)+
  facet_wrap(~ (t + 2008), ncol = 3) + 
  scale_fill_viridis_c(name = "", option = "plasma", breaks = c(0, 0.125, 0.25))

ggarrange(map_occ, map_sd, nrow = 2, common.legend = T, legend = "bottom")

### MAPS FOR LOCAL MODELS ------------------------------------------------------

plot.list <- pmap(list(dats_toplot[c(2,3,4)], grids[c(2,3,4)], c("pink", "lightblue", "lightgreen")), function(dat, grid, col){
  plot_map(dat, grid, estPsi > 0.5, show.legend = FALSE) +
    facet_wrap(~ (t + 2008), ncol = 3) + 
    scale_fill_manual(values = c("white", col))
})

helper <- ggplot()+
  geom_sf(data = st_union(grids[[1]])) +
  geom_sf(data = st_union(grids[[2]]), col = "pink", fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[3]]), col = "lightblue", fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[4]]), col = "lightgreen", fill = NA, linewidth = 2) +
  theme_void()

ggarrange(helper, plot.list[[1]], plot.list[[2]], plot.list[[3]],
          nrow = 2, ncol = 2, align = "hv")

### AVERAGE OCCUPANCY ----------------------------------------------------------

avg_occ <- get_avg_occ(dats[[1]], outs[[1]], XGAMs[[1]], dats$Fr$is.protected)

ggplot(avg_occ) +
  geom_line(aes(x = t, y = med, col = where)) + 
  geom_ribbon(aes(x = t, ymin = inf, ymax = sup, fill = where), alpha = 0.5)+
  xlab("time")+ ylab("Proportion of occupied cells")+
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
  scale_fill_manual(values = c("darkgrey", "pink", "lightblue", "lightgreen"))+
  scale_color_manual(values = c("darkgrey", "pink", "lightblue", "lightgreen"))+
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("") +
  theme_bw()

plot_d <- ggplot(pred_d) +
  geom_violin(aes(x=x, y=prob, fill = model), show.legend = F) +
  scale_fill_manual(values = c("darkgrey", "pink", "lightblue", "lightgreen"))+
  scale_color_manual(values = c("darkgrey", "pink", "lightblue", "lightgreen"))+
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("") +
  theme_bw()

ggarrange(plot_d, plot_c, nrow = 2, common.legend = T, legend = "right")


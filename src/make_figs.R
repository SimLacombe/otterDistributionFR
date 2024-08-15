library(tidyverse)
library(sf)
library(LaplacesDemon)
library(purrr)

rm(list = ls())

source("src/functions/plotting_functions.R")

years_toplot <- c(2010, 2015, 2020)

gradPsi <- scale_fill_gradient2(name = "psi",
                     low = "white",
                     mid = "orange",
                     high = "darkred",
                     midpoint = .5)

### LOAD MODEL OUTPUTS ---------------------------------------------------------

paths <- list(Fr = "out/Mod_full_20240803_232126.RData",
              NO = "out/Mod_NO_20240806_213836.RData",
              SE = "out/Mod_SE_20240807_030954.RData",
              E = "out/Mod_E_20240808_202739.RData")

envs <- map(paths, get_model)

outs <- map(envs, 1)
dats <- map(envs, 2)
grids <- map(envs, 3)
XGAMs <- map(envs, 4)

### COVARIATE EFFECTS ----------------------------------------------------------

pred_c <- map2(outs, grids, predict_c, off = log(100))
pred_c <- imap(pred_c, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind)

pred_d <- map(outs, predict_d, off = log(100))
pred_d <- imap(pred_d, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind)

plot_c <- ggplot(pred_c) +
  geom_line(aes(x=x, y=med, col = model)) +
  geom_ribbon(aes(x = x, col = model, fill = model,
                  ymin = inf, ymax = sup), alpha = 0.5, show.legend = F) +
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("") +
  theme_bw()

plot_d <- ggplot(pred_d) +
  geom_violin(aes(x=x, y=prob, fill = model)) +
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("") +
  theme_bw()

ggpubr::ggarrange(plot_d, plot_c, nrow = 2, common.legend = T, legend = "bottom")

### MAPS -----------------------------------------------------------------------

gridFull <- readRDS("data/L9310x10grid.rds")

dats <- pmap(list(dats, outs, XGAMs), get_psi)
  
grids <- map(grids, add_geom, gridFull)

dats <- map2(dats, grids, add_geom2)

dats_toplot <- map(dats, function(x){x %>% filter((t + 2008) %in% years_toplot)})

dat_toplot <- imap(dats_toplot, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind)

grid_toplot <- imap(grids, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind)

plot.list <- map(years_toplot, function(yr){
  map(names(envs), function(mod){
    plot_map(dat_toplot %>% filter(t + 2008 == yr, model == mod),
             grid_toplot %>% filter(model == mod), estPsi > 0.5) + 
      theme(legend.position = "none") +
      theme(aspect.ratio = 1)
    })
})

plot.list <- flatten(plot.list)

cowplot::ggdraw()+
  cowplot::draw_plot(plot.list[[1]], x = 0, y = 0, width = .66, height = 1) +
  cowplot::draw_plot(plot.list[[2]], x = 0.66, y = 0, width = .33, height = .5) +
  cowplot::draw_plot(plot.list[[3]], x = 0.66, y = 0.33, width = .33, height = .5)
# I need to arrange these plots better

### Plot all maps for appendix -------------------------------------------------

map.list <- map2(dats, grids, plot_map, estPsi)
map.list[[3]] + 
  facet_wrap(~ (t+2008)) +
  gradPsi

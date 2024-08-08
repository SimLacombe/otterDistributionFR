library(tidyverse)
library(sf)
library(LaplacesDemon)
library(purrr)

rm(list = ls())

source("src/functions/plotting_functions.R")

paths <- list(Fr = "out/Mod_full_20240803_232126.RData",
              NO = "out/Mod_NO_20240806_213836.RData")

grid <- readRDS("data/L9310x10grid_covs.rds")
grid$hydroLen <- c(scale(grid$hydroLen))
grid$ripProp <- c(scale(grid$ripProp))

envs <- map(paths, get_model)

outs <- map(envs, 1)
dats <- map(envs, 2)

### COVARIATE EFFECTS ----------------------------------------------------------

rip_values <- seq(min(grid$ripProp, na.rm = T),
              max(grid$ripProp, na.rm = T),
              length.out = 100)
hydr_values <- seq(min(grid$hydroLen, na.rm = T),
               max(grid$hydroLen, na.rm = T),
               length.out = 100)

pred <- matrix(0, nrow = 200, ncol = 4)
pred[1:100, 1] <- hydr_values
pred[101:200, 2] <- rip_values

pred_c <- map(outs, predict_c, pred = pred, off = log(100))
pred_c <- imap(pred_c, ~ .x %>% mutate(model = .y))
pred_c <- reduce(pred_c, rbind)

pred_d <- map(outs, predict_d, off = log(100))
pred_d <- imap(pred_d, ~ .x %>% mutate(model = .y))
pred_d <- reduce(pred_d, rbind)

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

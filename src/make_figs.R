library(tidyverse)
library(sf)
library(purrr)
library(ggpubr)
library(rjags)

rm(list = ls())

source("src/functions/plotting_functions.R")

years_toplot <- c(2009, 2012, 2015, 2018, 2021, 2023)

allOut <- new.env()

### Graphical params -----------------------------------------------------------

gradient_psi <- scale_fill_gradient2(name = bquote(psi),
                                     low = "white", mid = "orange", high = "darkred", midpoint = 0.5)

scale_contour <- scale_linewidth_manual(values = c("0.25" = 0.66, "0.5" = 1, "0.75" = 0.66),
                                        guide = "none")

colors_local <- c("plum2", "lightblue", "lightgreen")

### LOAD MODEL OUTPUTS ---------------------------------------------------------

paths <- list(Full = "out/modFr.RData",
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
                   "lambda_gam", "tau_gam", "sigma_ent")
  
allOut$diags <- map(outs, function(out){
  gelman.diag(as.mcmc.list(out[, params_toplot]))
})

# map(outs, function(out){
#   traceplot(as.mcmc.list(out[, params_toplot]))
# })

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

dats <- pmap(list(dats, outs, XGAMs), get_psi, nsplines = 15)
  
grids <- map(grids, add_geom, gridFull)

sp_dats <- map2(dats, grids, add_geom2)

dats_toplot <- map(sp_dats, function(x){x %>% filter((t + 2008) %in% years_toplot)})

contours <- map2(dats_toplot, map(dats_toplot, "t"), res = 40, get_contour_lines)

map_occ <- plot_map(dats_toplot[[1]], grids[[1]], estPsi)+
  geom_contour(data = contours[[1]], aes(x = X, y = Y, z = z, linewidth = factor(..level..)), 
               breaks = c(0.25, 0.5, 0.75), col = "black") +
  scale_contour +
  gradient_psi +
  facet_wrap(~ (t + 2008), ncol = 3)+ 
  xlab("")+ylab("")

map_sd <- plot_map(dats_toplot[[1]], grids[[1]], sdPsi) + 
  scale_fill_viridis_c(name = bquote(sd(psi)),
                       option = "plasma", breaks = c(0, 0.125, 0.25)) +
  facet_wrap(~ (t + 2008), ncol = 3) 

allOut$fig1 <- ggarrange(map_occ, map_sd, nrow = 2, align = "hv")

### MAPS FOR LOCAL MODELS ------------------------------------------------------

plot.list <- pmap(list(dats_toplot[c(2,3,4)], grids[c(2,3,4)], 
                       contours[c(2,3,4)], colors_local), 
                  function(dat, grid, contour, col){
                    plot_map(dat, grid, estPsi) +
                      geom_sf(data = st_union(grid), fill = NA, col = col, linewidth = 1) +
                      geom_contour(data = contour, aes(x = X, y = Y, z = z, linewidth = factor(..level..)), 
                                   breaks = c(0.25, 0.5, 0.75), col = "black") +
                      scale_contour +
                      gradient_psi +
                      facet_wrap(~ (t + 2008), ncol = 3) + 
                      xlab("")+ylab("")
})

helper <- ggplot()+
  geom_sf(data = st_union(grids[[1]])) +
  geom_sf(data = st_union(grids[[2]]), col = colors_local[1], fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[3]]), col = colors_local[2], fill = NA, linewidth = 2) +
  geom_sf(data = st_union(grids[[4]]), col = colors_local[3], fill = NA, linewidth = 2) +
  theme_void()

allOut$fig2 <- ggarrange(helper, plot.list[[3]], plot.list[[1]], plot.list[[2]],
          nrow = 2, ncol = 2, align = "hv", common.legend = T, legend = "bottom")

### AVERAGE OCCUPANCY ----------------------------------------------------------

avg_occ <- pmap(list(dats, outs, XGAMs, thr = 0.50, nsplines = 15), get_avg_occ)
avg_occ <- imap(avg_occ, ~ .x %>% mutate(model = .y)) %>%
  reduce(rbind) %>%
  mutate(model = factor(model, levels = names(envs)))

allOut$fig3 <- ggplot(avg_occ) +
  geom_ribbon(aes(x = t, ymin = inf, ymax = sup, fill = model, alpha = model))+
  geom_line(aes(x = t, y = med, col = model), linewidth = 1) + 
  xlab("time")+ ylab("Proportion of occupied cells") +
  scale_alpha_manual(values = c(0.75,0.33,0.33,0.33), name = "") +
  scale_fill_manual(values = c("darkgrey", colors_local), name = "") +
  scale_color_manual(values = c("darkgrey", colors_local), name = "") +
  facet_wrap(~ifelse(model == "Full", "Full model", "Local models")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

### COVARIATE EFFECTS ----------------------------------------------------------

lims <- c(min(grids$Full$hydroLen), max(grids$Full$hydroLen),
          min(grids$Full$ripProp), max(grids$Full$ripProp))

pred_c <- map(outs, predict_c, off = log(100), lims = lims)
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
  scale_fill_manual(values = c("darkgrey", colors_local), name = "") +
  scale_color_manual(values = c("darkgrey", colors_local), name = "") +
  facet_wrap(~ cov, scales = "free_x") +
  xlab("scaled covariate value") + ylab("occupancy probability") +
  theme_bw()+
  theme(legend.text = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

plot_d <- ggplot(pred_d) +
  geom_violin(aes(x=x, y=prob, fill = model), show.legend = F) +
  scale_fill_manual(values = c("darkgrey", colors_local), name = "") +
  scale_color_manual(values = c("darkgrey", colors_local), name = "") +
  facet_wrap(~ cov, scales = "free_x") +
  xlab("") + ylab("occupancy probability") +
  theme_bw() + 
  theme(axis.text.x = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

allOut$fig4 <- ggarrange(plot_d, plot_c, nrow = 2, common.legend = T, legend = "right")

### THINNING PROBABILITY -------------------------------------------------------

beta0_thin <- outs$Full[, grepl("beta0_thin", colnames(outs$Full)), drop = FALSE]

u_out <- outs$Full[, grepl("u_ent", colnames(outs$Full))] %>% 
  apply(2, function(x) quantile(invlogit(x + beta0_thin), probs = c(0.025, 0.5, 0.975))) %>%
  t

colnames(u_out) <- c("inf", "med", "sup")

thin_prob <- dats$Full %>% 
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

### All years  -------------------------------------------------------------------

contours2 <- map2(sp_dats, map(sp_dats, "t"), res = 40, get_contour_lines)

allOut$fig6 <- plot_map(sp_dats[[1]], grids[[1]], estPsi)+
  geom_contour(data = contours2[[1]], aes(x = X, y = Y, z = z, linewidth = factor(..level..)), 
               breaks = c(0.25, 0.5, 0.75), col = "black") +
  scale_contour +
  gradient_psi +
  facet_wrap(~ (t + 2008), ncol = 3)+ 
  xlab("")+ylab("")

allOut$fig7 <- plot_map(sp_dats[[2]], grids[[2]], estPsi)+
  geom_contour(data = contours2[[2]], aes(x = X, y = Y, z = z, linewidth = factor(..level..)), 
               breaks = c(0.25, 0.5, 0.75), col = "black") +
  scale_contour +
  gradient_psi +
  facet_wrap(~ (t + 2008), ncol = 3)+ 
  xlab("")+ylab("")

allOut$fig8 <- plot_map(sp_dats[[3]], grids[[3]], estPsi)+
  geom_contour(data = contours2[[3]], aes(x = X, y = Y, z = z, linewidth = factor(..level..)), 
               breaks = c(0.25, 0.5, 0.75), col = "black") +
  scale_contour +
  gradient_psi +
  facet_wrap(~ (t + 2008), ncol = 3)+ 
  xlab("")+ylab("")

allOut$fig9 <- plot_map(sp_dats[[4]], grids[[4]], estPsi)+
  geom_contour(data = contours2[[4]], aes(x = X, y = Y, z = z, linewidth = factor(..level..)), 
               breaks = c(0.25, 0.5, 0.75), col = "black") +
  scale_contour +
  gradient_psi +
  facet_wrap(~ (t + 2008), ncol = 3)+ 
  xlab("")+ylab("")

# save(allOut, file = "out/plots.RData")

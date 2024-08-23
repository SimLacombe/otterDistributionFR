
get_model <- function(path){
  load(path)
  list(out, ISDM_dat, L93_grid, gamDat$jags.data$X)
}

add_geom <- function(grid, gridFull){
  grid %>%
    left_join(gridFull) %>%
    st_as_sf(crs = 2154)
}

add_geom2 <- function(dat, grid){
  grid %>%
    select(px) %>%
    left_join(dat, .) %>%
    st_as_sf(crs = 2154)
}

plot_map <- function(dat, grid, arg, show.legend = TRUE){
  ggplot() +
    geom_sf(data = grid, fill = "lightgrey", col = NA) +
    geom_sf(data = dat, aes(fill = {{ arg }}), col = NA, show.legend = show.legend) +
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())
}

get_lam <- function(dat, out, XGAM){
  
  bbb <- out[, grep("b\\[", colnames(out))]
  bbl <- out[, grep("beta_latent\\[", colnames(out))]
  
  loglam <- map(seq_along(unique(dat$t)), function(.t){
    dat_t <- dat %>%
      filter(t == .t) 
    
    bbb[,(1:20) + (.t-1)*20] %*% t(XGAM[dat_t$px,]) +
      bbl %*% t(dat_t[, c("hydroLen", "ripProp", "Crayfish", "Trout")]) +
      matrix(rep(dat_t$logArea, nrow(out)),
             nrow = nrow(out),
             ncol = nrow(dat_t),
             byrow = TRUE)
  })
  
  rm(bbb)
  
  loglam <- reduce(loglam, cbind)
  exp(loglam)
}

get_psi <- function(dat, out, XGAM){
  
  lam <- get_lam(dat, out, XGAM)
  psi <- 1 - exp(-lam)
  
  dat %>% 
    mutate(estPsi = apply(psi, 2, mean),
           sdPsi = apply(psi, 2, sd))
}

get_avg_occ <- function(dat, out, XGAM, is.protected, quantiles = c(0.025,0.5,0.975)){
  
  lam <- get_lam(dat, out, XGAM)
  psi <- 1 - exp(-lam)
  
  avg_occ <- map(unique(dat$t), function(t){
    rbind(apply(psi[, which(dat$t == t)], 1, function(x){mean(as.numeric(x>0.5))}) %>% 
            quantile(quantiles),
          apply(psi[, which(dat$t == t & is.protected)], 1, function(x){mean(as.numeric(x>0.5))}) %>% 
            quantile(quantiles))
  }) %>%
    reduce(rbind)
  
  rownames(avg_occ) <- 1:nrow(avg_occ)
  colnames(avg_occ) <- c("inf", "med", "sup")
  
  data.frame(t = rep(unique(dat$t) + 2008, each = 2),
             where = rep(c("whole", "protected"), length(unique(dat$t))),
             avg_occ)
}

predict_c <- function(out, grid, off, quantiles = c(0.025,0.5,0.975)){
  
  rip_values <- seq(min(grid$ripProp, na.rm = T),
                    max(grid$ripProp, na.rm = T),
                    length.out = 100)
  hydr_values <- seq(min(grid$hydroLen, na.rm = T),
                     max(grid$hydroLen, na.rm = T),
                     length.out = 100)
  
  pred <- matrix(0, nrow = 200, ncol = 4)
  pred[1:100, 1] <- hydr_values
  pred[101:200, 2] <- rip_values
  
  Lam0 <- out[, grep("b\\[1,", colnames(out))] %>% 
    apply(1, mean)
  
  bbl <- out[, grep("beta_latent\\[", colnames(out))]
  
  predLam <- (Lam0 + bbl %*% t(pred) + log(100)) %>%
    exp 
  
  predPsi <- 1 - exp(-predLam) %>%
    apply(2, function(x){quantile(x, quantiles)})
  
  data.frame(inf = predPsi[1,],
             med = predPsi[2,],
             sup = predPsi[3,],
             x = c(pred[1:100, 1], pred[101:200, 2]),
             cov = rep(c("River len.", "Riparian prop."), each = 100))
}

predict_d <- function(out, off){
  
  pred <- matrix(0, 4, 4)
  pred[2,3] <- 1 
  pred[4,4] <- 1 
  
  Lam0 <- out[, grep("b\\[1,", colnames(out))] %>% 
    apply(1, mean)
  
  bbl <- out[, grep("beta_latent\\[", colnames(out))]
  
  predLam <- (Lam0 + bbl %*% t(pred) + log(100)) %>%
    exp 
  
  predPsi <- 1 - exp(-predLam)
  
  data.frame(Crayfish = c(predPsi[,1], predPsi[,2]),
             Trout = c(predPsi[,3], predPsi[,4]),
             x = rep(c("absent", "present"), each = nrow(predPsi))) %>%
    pivot_longer(cols = c("Crayfish", "Trout"), names_to = "cov", values_to = "prob")
}



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

get_psi <- function(dat, out, XGAM){
  
  bbb <- out[, grep("b\\[", colnames(out))]
  bbl <- out[, grep("beta_latent\\[", colnames(out))]
  
  ll <- map(seq_along(unique(dat$t)), function(.t){
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
  
  ll <- reduce(ll, cbind)
  
  dat %>% 
    mutate(estPsi = apply(1 - exp(-exp(ll)), 2, mean),
           sdPsi = apply(1 - exp(-exp(ll)), 2, sd))
}

plot_map <- function(dat, grid, arg){
  ggplot() +
    geom_sf(data = grid, fill = "lightgrey", col = NA) +
    geom_sf(data = dat, aes(fill = {{ arg }}), col = NA) +
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())
}

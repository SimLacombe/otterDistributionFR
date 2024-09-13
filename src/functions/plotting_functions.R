invlogit <- function(x){
  1/(1+exp(-x))
}

get_model <- function(path){
  load(path)
  list(mcmc, ISDM_dat, landscape, gamDat$jags.data$X)
}

add_geom <- function(grid, gridFull){
  grid %>%
    left_join(gridFull %>% select(gridCell)) %>%
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

get_contour_lines <- function(dat, groups = 1, res){

  dat_split <- split(dat, groups)
  
  contour_df <- map(names(dat_split), dats = dat_split, res = res, function(i, dats, res){
    boundary <- st_union(dats[[i]])
    centroids <- st_coordinates(st_centroid(dats[[i]]))
    interp_data <- akima::interp(x = centroids[, "X"], y = centroids[, "Y"], z = dats[[i]]$estPsi,
                                 linear = TRUE, duplicate = "mean", nx = res, ny = res)
    interp_sf <- data.frame(
      x = rep(interp_data$x, times = length(interp_data$y)),
      y = rep(interp_data$y, each = length(interp_data$x)),
      z = as.vector(interp_data$z)
    ) %>% st_as_sf(coords = c("x", "y"), crs = 2154)
    
    interp_clipped <- st_intersection(interp_sf, boundary)
    interp_clipped_df <- as.data.frame(st_coordinates(interp_clipped))
    interp_clipped_df$z <- interp_clipped$z
    interp_clipped_df$t <- as.numeric(i)
    
    interp_clipped_df
  })
  reduce(contour_df, rbind)
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

get_avg_occ <- function(dat, out, XGAM, thr, quantiles = c(0.025,0.5,0.975)){
  
  lam <- get_lam(dat, out, XGAM)
  psi <- 1 - exp(-lam)
  
  avg_occ <- map(unique(dat$t), function(t){
   apply(psi[, which(dat$t == t)], 1, function(x){mean(as.numeric(x>thr))}) %>%
      quantile(quantiles)
    
  }) %>%
    reduce(rbind)
  
  rownames(avg_occ) <- 1:nrow(avg_occ)
  colnames(avg_occ) <- c("inf", "med", "sup")
  
  data.frame(t = unique(dat$t) + 2008,
             avg_occ)
}

predict_c <- function(out, off, lims, quantiles = c(0.025,0.5,0.975)){
  
  rip_values <- seq(min(lims[3], na.rm = T),
                    max(lims[4], na.rm = T),
                    length.out = 100)
  hydr_values <- seq(min(lims[1], na.rm = T),
                     max(lims[2], na.rm = T),
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


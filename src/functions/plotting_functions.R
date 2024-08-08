
get_model <- function(path){
  load(path)
  list(out, ISDM_dat)
}

predict_c <- function(out, pred, off, quantiles = c(0.025,0.5,0.975)){
  
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

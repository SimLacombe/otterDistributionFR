model{
  ## state model 
  for( m in 1:nsite ){
    z[m] ~ dcat(c(1-psi, psi))}
  
  ## detection model 
  for( m in 1:nsite ){
    for( t in 1:nseason ){
      for( k in 1:nsurvey ){
        y[m, t, k] ~ dcat(rho.mat[z[m],])
      }}}
  
  rho.mat[1,1] <- 1
  rho.mat[1,2] <- 0
  rho.mat[2,1] <- 1 - rho 
  rho.mat[2,2] <- rho
    
  ## linear predictors
  psi <- ilogit(alp0)
  rho <- ilogit(bet0)
  
  ## prioris
  alp0 ~ dlogis(0, 1)
  bet0 ~ dlogis(0, 1)
}

sites <- unique(otterDat$grid.cell)
M <- length(sites)
years <- unique(otterDat$year)
T <- length(years)
K <- max(otterDat$obs)

y <- array(NA, dim = c(M,T,K))
for(m in 1:M){
  for(t in 1:T){
    y.mt <- otterDat$presence[otterDat$grid.cell == sites[m]&otterDat$year == years[t]]
    if(!(is_empty(y.mt)))
      y[m,t,1:length(y.mt)] <- y.mt
  }
}

# data.list <- list( y = y + 1,
#                    nsite = M,
#                    nseason = T,
#                    nsurvey = K )
# 
# z0 <- as.numeric(apply(y, 1, FUN = function(x){any(x, na.rm = T)})) + 1 
# 
# inits <- list(list(alp0 = logit(0.5), bet0 = logit(0.5), z = z0),
#               list(alp0 = logit(0.5), bet0 = logit(0.5), z = z0),
#               list(alp0 = logit(0.5), bet0 = logit(0.5), z = z0),
#               list(alp0 = logit(0.5), bet0 = logit(0.5), z = z0))
# 
# mod <- run.jags(model = "src/toy_occmod.R",
#                 monitor = c("alp0", "bet0", "z"),
#                 data = data.list,
#                 n.chains = 4,
#                 inits = inits,
#                 adapt = 500,
#                 burnin = 1000,
#                 sample = 1000,
#                 thin = 1,
#                 summarise = TRUE,
#                 plots = TRUE,
#                 method = "parallel")
# 
# M.mat <- foreach( i = 1:4, .combine = rbind )%do%{
#   cbind(CHAIN = i,as.mcmc.list(mod)[[i]])
# }
# 
# z.est <- data.frame(grid.cell = sites)
# z.est$occ <- M.mat[, grep("z\\[", colnames(M.mat))] %>% 
#   apply(2, FUN = function(x){mean(x-1)})

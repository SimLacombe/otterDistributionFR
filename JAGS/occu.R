model{
  ## LATENT MODEL ## 
  for(t in 1:nyear){
    for(pixel in 1:npixel){
      cloglog(psi[pixel, t]) <- inprod(x_latent[pixel,], beta_latent) + inprod(x_gam[pixel, ], b[, t]) + cell_area[pixel]
      z[pixel, t] ~ dbern(psi[pixel, t])
    } 
  }
  
  ## PA DATA MODEL ##
  for(t in 1:nyear){
    for(pa in (pa.idxs[t]+1):pa.idxs[t+1]){
      y[pa] ~ dbin(z[pa_pixel[pa], t] * rho[pa_pixel[pa]], K[pa])
    }
  }
  
  ## LINEAR PREDICTORS
  for(pixel in 1:npixel){
    logit(rho[pixel]) <-inprod(x_rho[pixel, ], beta_rho)
  }
  
  ## GAM STUFF ## 
  K1 <- S1[1:(nspline-1),1:(nspline-1)] * lambda_gam
  
  b[1, 1] ~ dnorm(-4.6, 0.75)
  b[2:nspline, 1] ~ dmnorm(zero[2:nspline],K1)
  
  for(jj in 1:nspline){
    for(yr in 2:nyear){
      b[jj,yr] ~ dnorm(b[jj,yr-1], tau_gam)
    }
  }
  
  ## PRIORS ##
  for(cov in 1:ncov_lam){
    beta_latent[cov] ~ dlogis(logit(0.05), 1)
  }
  for(cov in 1:ncov_rho){
    beta_rho[cov] ~ dlogis(0, 1)
  }
  
  lambda_gam ~ dgamma(.05,.005)
  tau_gam ~ dgamma(1,1)
}
  
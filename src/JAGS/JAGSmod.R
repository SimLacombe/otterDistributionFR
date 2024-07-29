model{
  ## LATENT MODEL ## 
  for(pxt in 1:npxt){
    log(lambda[pxt]) <- inprod(x_latent[pxt,], beta_latent) + inprod(x_gam[px[pxt], ], b[, t[pxt]])
    log(lambda_B[pxt]) <- log(lambda[pxt]) + cell_area[pxt]
    psi[pxt] <- 1 - exp(-lambda_B[pxt])
    z[pxt] ~ dbern(psi[pxt])
  }
  
  ## OBSERVATION MODEL MODEL ##
  #1. Presence absence 
  # K number of visits, ypa number of positive visits
  # pxts_pa : indices of pixels with at least one pa visit
  for(pxt in pxts_pa){ 
    ypa[pxt] ~ dbin(z[pxt] * rho[pxt, pa_protocol[pxt]], K[pxt])
  }
  
  # 2. Presence only
  for(pxt in pxts_po){ 
    ones[pxt] ~ dbern(exp(-lambda_B[pxt] * thin_prob[pxt]) * (lambda[pxt] * thin_prob[pxt]) ** ypo[pxt])
  }
  for(pxt in pxts_po_no){ 
    ones[pxt] ~ dbern(exp(-lambda_B[pxt] * thin_prob[pxt]))
  }
  
  ## LINEAR PREDICTORS
  for(pxt in 1:npxt){
    thin_prob[pxt] <- ilogit(inprod(x_thin[pxt, ], beta_thin)) + beta_ent[ent[px[pxt], t[pxt]], t[pxt]]
    for(protocol in 1:nprotocols){
      rho[pxt, protocol] <- ilogit(inprod(x_rho[pxt, ], beta_rho) + beta_rho_protocol[protocol])
    }
  }
  
  ## GAM ##
  K1 <- S1[1:(nspline-1),1:(nspline-1)] * lambda_gam
  
  b[1, 1] ~ dnorm(-4.6, 0.75)
  b[2:nspline, 1] ~ dmnorm(zero[2:nspline],K1)
  
  for(jj in 1:nspline){
    for(yr in 2:nyear){
      b[jj,yr] ~ dnorm(b[jj,yr-1], tau_gam)
    }
  }
  
  ## RANDOM EFFECTS ##
  
  for(protocol in 1:nprotocols){
    beta_rho_protocol[protocol] ~ dnorm(0, 1/(sigma_protocol*sigma_protocol))
  }
  
  for(ent in 1:nent){
    for(yr in 1:nyear){
      beta_ent[ent, yr] ~ dnorm(0, 1/(sigma_ent*sigma_ent))
    }
  }
  
  ## PRIORS ##
  for(cov in 1:ncov_lam){
    beta_latent[cov] ~ dnorm(0, 0.01)
  }
  for(cov in 1:ncov_thin){
    beta_thin[cov] ~ dlogis(0, 1)
  }
  for(cov in 1:ncov_rho){
    beta_rho[cov] ~ dlogis(0, 1)
  }

  sigma_protocol ~ dunif(0,100)
  sigma_ent ~ dunif(0,100)
  
  lambda_gam ~ dgamma(.05,.005)
  tau_gam ~ dgamma(1,1)
}



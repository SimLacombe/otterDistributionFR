model{
  ## LATENT MODEL ## 
  for(pxt in 1:npxt){
    log(lambda[pxt]) <- inprod(x_latent[px[pxt],], beta_latent) + inprod(x_gam[px[pxt], ], b[, t[pxt]]) + cell_area[px[pxt]]
    psi[pxt] <- 1 - exp(-lambda[pxt])
    z[pxt] ~ dbern(psi[pxt])
  }
  
  ## OBSERVATION MODEL MODEL ##
  #1. Presence absence
  for(pxt in pxts_pa){ 
    ypa[pxt] ~ dbin(z[pxt] * rho[pxt, pa_protocol[pxt]], K[pxt])
  }
  
  # 2. Presence only
  # when ypo > 0 I use the Poisson variable
  for(pxt in pxts_po){ 
    ypo[pxt] ~ dpois(lambda[pxt] * thin_prob[pxt])
  }
  # When ypo = 0, I explicitely calculate the ll of the poisson variable (saves a lot of time)
  for(pxt in pxts_po_no){ 
    ones[pxt] ~ dbern(exp(-lambda[pxt] * thin_prob[pxt]))
  }
  
  ## LINEAR PREDICTORS
  for(pxt in 1:npxt){
    thin_prob[pxt] <- ilogit(inprod(x_thin[px[pxt], ], beta_thin)) + beta_region[region[pxt], t[pxt]]
    for(protocol in 1:nprotocols){
      rho[pxt, protocol] <- ilogit(inprod(x_rho[px[pxt], ], beta_rho) + beta_rho_protocol[protocol])
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
  
  for(reg in 1:nregion){
    for(yr in 1:nyear){
      beta_region[reg, yr] ~ dnorm(0, 1/(sigma_region*sigma_region))
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
  sigma_region ~ dunif(0,100)
  
  lambda_gam ~ dgamma(.05,.005)
  tau_gam ~ dgamma(1,1)
}



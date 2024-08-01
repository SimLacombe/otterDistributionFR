model{
  ## LATENT MODEL ## 
  for(pxt in 1:npxt){
    log(lambda[pxt]) <- inprod(x_latent[pxt,], beta_latent) + inprod(x_gam[px[pxt], ], b[, t[pxt]]) + cell_area[pxt]
    psi[pxt] <- 1 - exp(-lambda[pxt])
    z[pxt] ~ dbern(psi[pxt])
  }
  
  ## OBSERVATION MODEL MODEL ##
  #1. Presence absence 
  # K number of visits, ypa number of positive visits
  # pxts_pa : indices of pixels with at least one pa visit
  for(pxt in pxts_pa){ 
    rho[pxt] <- ilogit(beta0_rho + u_protocol[pa_protocol[pxt]])
    ypa[pxt] ~ dbin(z[pxt] * rho[pxt], K[pxt])
  }
  
  # 2. Presence only
  for(pxt in pxts_po){ 
    thin_prob[pxt] <- ilogit(beta0_thin + u_ent[ent.yr[pxt]])
    ypo[pxt] ~ dpois(lambda[pxt] * thin_prob[pxt])
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
    u_protocol[protocol] ~ dnorm(0, 1/(sigma_protocol*sigma_protocol))
  }
  
  for(ent in 1:nent){
      u_ent[ent] ~ dnorm(0, 1/(sigma_ent*sigma_ent))
    }
  
  ## PRIORS ##
  for(cov in 1:ncov_lam){
    beta_latent[cov] ~ dnorm(0, 0.01)
  }
  
  beta0_thin ~ dlogis(0, 1)
  beta0_rho ~ dlogis(0, 1)

  sigma_protocol ~ dunif(0,100)
  sigma_ent ~ dunif(0,100)
  
  lambda_gam ~ dgamma(.05,.005)
  tau_gam ~ dgamma(1,1)
}



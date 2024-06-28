### DATA
  # - npx : integer - number of data points (number of pixel x year sampled)
  # - npixel : integer - total number of pixels in the study area (sampled or not)
  # - nyear : integer - number of years
  # - px : integer(length = npx) - pixel index of each pixel x year
  # - t : integer(length = npx) - time period of each pixel x year
  # - ypa : integer(length = npx) - number of pa observations for each pixel x year
  # - K : integer(length = npx) - number of pa visits for each pixel x year
  # - ypo : integer(length = npx) - number of po observations for each pixel x year
  # - is_po_sampled : integer(length = npx) - is the pixel x year sampled for presence only ? (0/1)
  # - nprotocols : integer - number of pa protocols 
  # - pa_protocol : integer(length = npx) - pa protocol used in each pixel x year
  # - ncov_lam : integer - number of covariates for lambda
  # - ncov_thin : integer - number of covariates for the thinning probability
  # - ncov_rho : integer - number of covariates for rho
  # - cell_area : numeric(length = npx) - log area of each pixel 
  # - x_latent : numeric matrix(npx, ncov_lam) - covariate matrix for lambda
  # - x_thin : numeric matrix(npx, ncov_lam) - covariate matrix for the thin. prob. 
  # - x_rho : numeric matrix(npx, ncov_lam) - covariate matrix for rho
  # - nsplines : integer - number of splines
  # - x_gam : numeric matrix(npx, nsplines) - matrix of spline values
  # - S1 : numeric matrix(nsplines, nsplines) - covariance matrix for the spline coefficients
  # - zero : numeric matrix(nsplines, nsplines) - matrix of zeroes
    
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
  # when ypo > 0 I use a Poisson variable
  for(pxt in pxts_po){ 
    ypo[pxt] ~ dpois(lambda[pxt] * thin_prob[pxt])
  }
  # When ypo = 0, I explicitely calculate the ll of a poisson variable (saves a lot of time)
  for(pxt in pxts_po_no){ 
    ones[pxt] ~ dbern(exp(-lambda[pxt] * thin_prob[pxt]))
  }
  
  ## LINEAR PREDICTORS
  for(pxt in 1:npxt){
    thin_prob[pxt] <- ilogit(inprod(x_thin[px[pxt], ], beta_thin))
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
  
  lambda_gam ~ dgamma(.05,.005)
  tau_gam ~ dgamma(1,1)
}



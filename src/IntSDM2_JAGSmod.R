# INPUTS:
#   cell_area: log area of grid cells
#   nyear: number of years 
#   npixel: number of grid cells
#   npo: number of presence-only datapoints for each time step
#   po.idxs: indexes of last po point for each year (first index is 0)
#   nsite: number of presence-absence sites
#   x_psi: occupancy covariates
#   x_lam: intensity covariates
#   x_b: presence-only detection covariates
#   x_rho: presence-absence detection covariates
#   x_gam: colonization probability covariates
#   ncov_psi: number of covariates for rho
#   ncov_lam: number of covariates for lambda
#   ncov_b: number of covariates for b
#   ncov_rho: number of covariates for rho
#   ncov_gam: number of covariates for rho
#   po_pixel: pixel number of each presence-only datapoint
#   pa_pixel: pixel number of each presence-absence datapoint
#   y: presence-absence dataset
#   K: number of secondary sampling occasions for each presence-absence site and time step
#   ones: vector of 1 same length as PO dataset
#   cste: constraining constant to keep the presence-only likelihood in [0,1]
#
# OUTPUTS:
#   lambda[npixel]: estimated latent intensity
#   z[npixel]: estimated latent occupancy 
#   b[npixel]: estimated presence-only detection probabilities
#   rho[nsite]: estimated presence-absence detection probabilities
#   alpha: logit-linear predictors for psi
#   beta_psi: logit-linear predictors for psi
#   beta_lam: log-linear predictors for lambda
#   beta_b: logit-linear predictors for b
#   beta_rho: logit-linear predictors for rho


model{
  ## LATENT MODEL ## 
  for(pixel in 1:npixel){
    
    logit(psi[pixel]) <- inprod(x_psi[pixel,], beta_psi)
    logit(b[pixel]) <- inprod(x_b[pixel,], beta_b)
    log(lambda[pixel]) <- inprod(x_lam[pixel,], beta_lam) + cell_area[pixel]
    
    z[pixel, 1] ~ dbern(psi[pixel])
    lambda_corr[pixel, 1] <- lambda[pixel] * z[pixel, 1]
    for(t in 2:nyear){
      z[pixel, t] ~ dbern(z[pixel, t-1] + gamma[pixel, t] * (1 - z[pixel, t-1]))
      lambda_corr[pixel, t] <- lambda[pixel] * z[pixel, t]
      } 
  }

  ## PRESENCE-ONLY DATA MODEL ##
  for(t in 1:nyear){
    po_denominator[t] <- inprod(lambda_corr[1:npixel, t], b[1:npixel] ) / npo[t]
    for(po in (po.idxs[t]+1):po.idxs[t+1]){
      ones[po] ~ dbern(
        exp(
          log(lambda_corr[po_pixel[po], t]*b[po_pixel[po]]) -
            log(po_denominator[t])
        ) / cste
      )
    } 
  }

  ## PRESENCE-ABSENCE DATA MODEL ##
  for(site in 1:nsite){
    logit(rho[site]) <-inprod(x_rho[pa_pixel[site], ], beta_rho)
    for(t in 1:nyear){
      y[site, t] ~ dbin(z[pa_pixel[site], t] * rho[site], K[site, t])
    }
  }
  
  ## CALCULATION OF GAMMA BASED ON THE DISTANCE MATRIX ##


  for(pixel in 1:npixel){
    gamma[pixel, 1] <- gamma0
    for(t in 2:nyear){
      # gamma[pixel, t] <- 1 - prod(1 - inprod(z[, t-1], gamma0 * D[pixel, ]**(1/(2*sigma**2) )))
      gamma[pixel, t] <- gamma0
    }
  }
  
  ## PRIORS ##
  for(occu in 1:ncov_psi){
    beta_psi[occu] ~ dlogis(0, 1)
  }
  for(cov in 1:ncov_lam){
    beta_lam[cov] ~ dnorm(0, 0.01)
  }
  for(cov in 1:ncov_b){
    beta_b[cov] ~ dlogis(0, 1)
  }
  for(cov in 1:ncov_rho){
    beta_rho[cov] ~ dlogis(0, 1)
  }
  gamma0 ~ dunif(0, 1)
  # sigma ~ dunif(0,100)
}



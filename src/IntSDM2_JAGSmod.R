# INPUTS:
#   cell_area: log area of grid cells
#   npixel: number of grid cells
#   npo: number of presence-only datapoints
#   nsite: number of presence-absence sites
#   K: number of secondary sampling occasions for each presence-absence site
#   x_psi: occupancy covariates
#   x_lam: intensity covariates
#   x_b: presence-only detection covariates
#   x_rho: presence-absence detection covariates
#   ncov_lam: number of covariates for lambda
#   ncov_b: number of covariates for b
#   ncov_rho: number of covariates for rho
#   po_pixel: pixel number of each presence-only datapoint
#   pa_pixel: pixel number of each presence-absence datapoint
#   y: presence-absence dataset
#   ones: vector of 1 same length as PO dataset
#   cste: constraining constant to keep the presence-only likelihood in [0,1]
#
# OUTPUTS:
#   lambda[npixel]: estimated latent intensity
#   z[npixel]: estimated latent occupancy 
#   b[npixel]: estimated presence-only detection probabilities
#   rho[nsite]: estimated presence-absence detection probabilities
#   alpha: logit-linear predictors for psi
#   beta: log-linear predictors for lambda
#   gamma: logit-linear predictors for b
#   delta: logit-linear predictors for rho


model{
  ## LATENT MODEL ## 
  for(pixel in 1:npixel){
    lambda[pixel] <- exp(inprod(x_lam[pixel,], beta) + cell_area[pixel]) * z[pixel]
    logit(psi[pixel]) <- inprod(x_psi[pixel,], alpha)
    logit(b[pixel]) <- inprod(x_b[pixel,], gamma)
    z[pixel] ~ dbern(psi[pixel])
  } 
  
  ## PRESENCE-ONLY DATA MODEL ##
  po_denominator <- inprod(lambda[1:npixel], b[1:npixel] ) / npo
  for(po in 1:npo){
    ones[po] ~ dbern(
      exp(
        log(lambda[po_pixel[po]]*b[po_pixel[po]]) -
          log(po_denominator)
      ) / cste
    )
  } 
  ## PRESENCE-ABSENCE DATA MODEL ##
  for(site in 1:nsite){
    logit(rho[site]) <-inprod(x_rho[pa_pixel[site], ], delta)
    y[site] ~ dbin(z[pa_pixel[site]] * rho[site], K[site])
  }
  
  ## PRIORS ##
  for(occu in 1:ncov_psi){
    alpha[occu] ~ dlogis(0, 1)
  }
  for(intens in 1:ncov_lam){
    beta[intens] ~ dnorm(0, 0.01)
  }
  for(po in 1:ncov_b){
    gamma[po] ~ dlogis(0, 1)
  }
  for(pa in 1:ncov_rho){
    delta[pa] ~ dlogis(0, 1)
  }
}



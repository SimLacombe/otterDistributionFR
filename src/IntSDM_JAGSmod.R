# INPUTS:
#   cell_area: log area of grid cells
#   npixel: number of grid cells
#   N_po: number of presence-only datapoints
#   nsite: number of presence-absence sites
#   K: number of secondary sampling occasions for each presence-absence site
#   x_lam: latent state covariates
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
#   beta: log-linear predictors for lambda
#   alpha: logit-linear predictors for b
#   gamma: logit-linear predictors for rho


model{
  ## LATENT MODEL ## 
  for(pixel in 1:npixel){
    log(lambda[pixel]) <-inprod(x_lam[pixel,], beta) + cell_area
    z[pixel] ~ dbern(1 - exp(-lambda[pixel]))
    logit(b[pixel]) <-  inprod(x_b[pixel,], alpha)
  } 
  
  ## PRESENCE-ONLY DATA MODEL ##
  po_denominator <- inprod(lambda[1:npixel], b[1:npixel] ) / N_po
  for(po in 1:N_po){
    ones[po] ~ dbern(
      exp(
        log(lambda[po_pixel[po]]*b[po_pixel[po]]) -
          log(po_denominator)
      ) / cste
    )
  } 
  ## PRESENCE-ABSENCE DATA MODEL ##
  for(site in 1:nsite){
    logit(rho[site]) <-inprod(x_rho[pa_pixel[site], ], gamma)
    y[site] ~ dbin(z[pa_pixel[site]] * rho[site], K[site])
  }
  
  ## PRIORS ##
  for(latent in 1:ncov_lam){
    beta[latent] ~ dnorm(0, 0.01)
  }
  for(po in 1:ncov_b){
    cc[po] ~ dlogis(0, 1)
  }
  for(pa in 1:ncov_rho){
    a[pa] ~ dlogis(0, 1)
  }

}



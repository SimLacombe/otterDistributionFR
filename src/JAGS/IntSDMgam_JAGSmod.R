# INPUTS:
#   cell_area: log area of grid cells
#   npixel: number of grid cells
#   npo: number of presence-only datapoints
#   nsite: number of presence-absence sites
#   K: number of secondary sampling occasions for each presence-absence site
#   x_latent: latent state covariates
#   x_thin: presence-only detection covariates
#   x_rho: presence-absence detection covariates
#   ncov_lam: number of covariates for lambda
#   ncov_thin: number of covariates for thin_prob
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
#   thin_prob[npixel]: estimated presence-only detection probabilities
#   rho[nsite]: estimated presence-absence detection probabilities
#   u[npixel]: estiamted spatial random effect on lambda
#   beta_lam: log-linear predictors for lambda
#   beta_thin: logit-linear predictors for b
#   beta_rho: logit-linear predictors for rho


model{
  ## LATENT MODEL ## 
  for(t in 1:nyear){
    for(pixel in 1:npixel){
      log(lambda[pixel, t]) <- inprod(x_latent[pixel,], beta_latent) + inprod(x_gam[pixel, ], b[, t]) + cell_area[pixel]
      psi[pixel, t] <- 1 - exp(-lambda[pixel,t])
      z[pixel, t] ~ dbern(psi[pixel, t])
    } 
  }

  
  ## PRESENCE-ONLY DATA MODEL ##
  for(t in 1:nyear){
    po_denominator[t] <- inprod(lambda[1:npixel, t], thin_prob[1:npixel, t] ) / npo[t]
    for(po in (po.idxs[t]+1):po.idxs[t+1]){
      ones[po] ~ dbern(
        exp(
          log(lambda[po_pixel[po], t]*thin_prob[po_pixel[po], t]) -
            log(po_denominator[t])
        ) / cste
      )
    } 
  }
  
  ## PRESENCE-ABSENCE DATA MODEL ##
  for(t in 1:nyear){
    for(pa in (pa.idxs[t]+1):pa.idxs[t+1]){
      y[pa] ~ dbin(z[pa_pixel[pa], t] * rho[pa_pixel[pa], pa_protocole[pa]], K[pa])
    }
  }
  
  ## LINEAR PREDICTORS
  for(pixel in 1:npixel){
    for(t in 1:nyear){
      logit(thin_prob[pixel, t]) <- inprod(x_thin[pixel,], beta_thin) + beta_region[region[pixel], t]
    }
    logit(rho[pixel, 1]) <-inprod(x_rho[pixel, ], beta_rho)
    logit(rho[pixel, 2]) <-inprod(x_rho[pixel, ], beta_rho) + beta_rho_point
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
  
  # RANDOM EFFECTS ##
  for(reg in 1:nregion){
    for(t in 1:nyear){
      beta_region[reg, t] ~ dnorm(0, 1/(sigma_region*sigma_region))
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
    beta_rho_point[cov] ~ dlogis(0, 1)
  }

  sigma_region ~ dunif(0,100)
  
  lambda_gam ~ dgamma(.05,.005)
  tau_gam ~ dgamma(1,1)
}



my_inits <- function(chain){
  gen_list <- function(chain = chain){
    list(
      z = rep(1, data.list$npxt),
      beta_latent = rnorm(data.list$ncov_lam, 0, 0.25),
      beta0_rho = rnorm(1, 0, 0.25),
      beta0_thin = rnorm(1, 0, 0.25),
      b = matrix(
        rnorm(data.list$nspline * data.list$nyear, rep(gamDat$jags.ini$b,each = data.list$nyear),0),
        nrow = data.list$nspline,
        ncol = data.list$nyear, byrow = TRUE),
      lambda_gam = gamDat$jags.ini$lambda,
      tau_gam = rgamma(1,1,1),
      sigma_protocol = runif(1, 0, 10),
      sigma0_ent = runif(1, 0, 10),
      sigma1_ent = runif(1, 0, 10),
      sigma2_ent = runif(1, 0, 10),
      .RNG.name = switch(chain,
                         "1" = "base::Wichmann-Hill",
                         "2" = "base::Wichmann-Hill",
                         "3" = "base::Super-Duper",
                         "4" = "base::Mersenne-Twister",
                         "5" = "base::Wichmann-Hill",
                         "6" = "base::Marsaglia-Multicarry",
                         "7" = "base::Super-Duper",
                         "8" = "base::Mersenne-Twister"),
      .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain),
                "5" = gen_list(chain),
                "6" = gen_list(chain),
                "7" = gen_list(chain),
                "8" = gen_list(chain)
  )
  )
}

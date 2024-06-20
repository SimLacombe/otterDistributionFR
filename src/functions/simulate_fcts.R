gen_mvn <- function(grid = NULL, mu = NULL,
                    sigma = NULL, rho = NULL){
  # get bounds of raster
  bounds <- st_bbox(grid)
  
  # input a proportion of where you want mu to be on x and y
  mu_loc <- c(bounds$xmin + mu[1] * (bounds$xmax - bounds$xmin),
              bounds$ymin + mu[2] * (bounds$ymax - bounds$ymin))
  
  Sigma <- diag(c(sigma[1] * abs(bounds$xmax - bounds$xmin),
                  sigma[2] * abs(bounds$ymax - bounds$ymin)))
  # fill the off diagonal
  Sigma[2:3] <- rep(rho * prod(diag(Sigma)))
  
  values <- dmvnorm(st_coordinates(st_centroid(grid)), 
                       mean=mu_loc, 
                       sigma=Sigma)
  
  values / max(values) * 5
}
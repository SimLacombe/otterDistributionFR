getSites <- function(xVec, yVec, thr){
  
  coords <- st_as_sf(data.frame(x = xVec, y = yVec),
                     coords = c("x", "y"), crs = 2154)
  
  A <- matrix(0, nrow(coords), nrow(coords))
  A[as.numeric(st_distance(coords, coords)) < thr] <- 1
  
  concomFromMatAdj(A)[[1]]
}

collapse_transects <- function(times, coords, thr.space, thr.time){
  
  A.space <- matrix(0, length(coords), length(coords))
  A.space[as.numeric(st_distance(coords, coords)) < thr.space] <- 1
  
  A.time <- matrix(0, length(coords), length(coords))
  A.time[sapply(times, FUN = function(x){abs(difftime(x, times, units = "days"))}) < thr.time] <- 1
  
  concomFromMatAdj(A.time * A.space)[[1]]
}
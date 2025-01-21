getSites <- function(xVec, yVec, thr){
  
  coords <- st_as_sf(data.frame(x = xVec, y = yVec),
                     coords = c("x", "y"), crs = 2154)
  
  A <- matrix(0, nrow(coords), nrow(coords))
  A[as.numeric(st_distance(coords, coords)) < thr] <- 1
  
  concomFromMatAdj(A)[[1]]
}

getSamplingArea <- function(x, y, res = 100, bw = 25000, offset = st_bbox(map_FR)[c(1,3,2,4)] + c(-1,1,-1,1)*25000, lvl){
  kde <- MASS::kde2d(x, y, n = res, h = c(bw, bw), lims = offset)
  
  contour_values <- sort(kde$z)
  contour_level <- contour_values[which.max(cumsum(contour_values) / sum(contour_values) >= lvl)]
  contour_lines <- contourLines(x = kde$x, y = kde$y, z = kde$z, levels = contour_level)
  
  contour_sf <- do.call(st_sfc, lapply(contour_lines, function(contour) {
    coords <- cbind(contour$x, contour$y)
    st_polygon(list(coords))
  })) %>% st_union()
  
  contour_sf
}

subsample <- function(geometry, thr){
  
  ids_toprocess <- seq_along(geometry)
  ids_kept <- integer()
  
  while(length(ids_toprocess) > 1){
    id <- sample(ids_toprocess, 1)
    ids_kept <- append(ids_kept, id)
    distances <- st_distance(geometry, geometry[id]) %>%
      units::set_units(km) %>%
      as.numeric()
    
    ids_toprocess <- ids_toprocess[ids_toprocess %in% which(distances >= thr)]
  }
  append(ids_kept, ids_toprocess)
}

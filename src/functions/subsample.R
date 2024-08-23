subsample <- function(geometry, thr){
  
  ids_toprocess <- seq_along(geometry)
  ids_kept <- integer()
  
  while(length(ids_toprocess) > 1){
    id <- sample(ids_toprocess, 1)
    ids_kept <- append(ids_kept, id)
    distances <- st_distance(geometry, geometry[id]) %>%
      units::set_units(km) %>%
      as.numeric()
    
    ids_toprocess <- ids_toprocess[ids_toprocess %in% which(distances > thr)]
  }
  append(ids_kept, ids_toprocess)
}
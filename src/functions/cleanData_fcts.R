addProtocole <- function(data, colNames, patterns, protName) {
  
  if (length(colNames) != length(patterns)) {
    stop("The number of columns given doesn't match the number of patterns")
  }
  
  out <- rep(TRUE, nrow(data))
  
  for (i in seq_along(colNames)) {
    out <- out & strSearch(data[[colNames[i]]], patterns[i])
  }
  
  new_data <- data %>%
    mutate(!!protName := out)
  
  return(new_data)
}

strSearch <- function(comm, pattern) {
  split <- strSplit(pattern)
  patterns <- split$patterns
  links <- split$links
  
  out <- grepl(patterns[1], comm)
  
  for (i in seq_along(links)) {
    if (links[i] == "&") {
      out <- out & grepl(patterns[i + 1], comm)
    }
    else if (links[i] == "&!") {
      out <- out & !grepl(patterns[i + 1], comm)
    }
    else{
      stop(paste0("unknown logical connector: ", links[i]))
    }
  }
  return(out)
}

strSplit <- function(str) {
  bricks <- strsplit(str, "_")[[1]]
  if(length(bricks) == 1){
    patterns <- bricks
    links <- character(0)
  }
  else{
  patterns <- bricks[seq(1, length(bricks), by = 2)]
  links <- bricks[seq(2, length(bricks), by = 2)]
  }
  return(list(patterns = patterns, links = links))
}

### Perform protocol attribution based on keywords search ---------------------

addProtocol <- function(x, patterns, protocol, col1, col2 ) {
    x %>%
      mutate(cond1 = strSearch({{ col1 }}, patterns[1]),
             cond2 = strSearch({{ col2 }}, patterns[2]),
             cond2 = ifelse(is.na(cond2), TRUE, cond2)) %>%
      mutate(!!enquo(protocol) := cond1&cond2, .keep = "unused")
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

### keep a single column per protocole -----------------------------------------

arrangeProtocols <- function(x, ...){
  tmp <- x %>% 
    select(...)
  names <- names(tmp)
  x %>% 
    select(- c(...)) %>%
    mutate(protocol = apply(tmp, 1, function(x){names[which(x)[1]]}))
}

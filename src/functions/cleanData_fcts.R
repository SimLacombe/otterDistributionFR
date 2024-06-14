### Perform protocol attribution based on keywords search ----------------------

addProtocol <- function(x, patterns, protocol, col1, col2 ) {
  if(length(patterns) == 0){
    x <- x %>% 
      mutate(!!enquo(protocol) := TRUE)
  }
  else{
    x <- x %>%
      mutate(cond1 = strSearch({{ col1 }}, patterns[1]),
             cond2 = strSearch({{ col2 }}, patterns[2]),
             cond2 = ifelse(is.na(cond2), TRUE, cond2)) %>%
      mutate(!!enquo(protocol) := cond1&cond2, .keep = "unused")
  }
    x
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
    mutate(protocol = apply(tmp, 1, function(x){names[which(x)[1]]})) %>%
    filter(!is.na(protocol))
}

### remove Camera-trap data ----------------------------------------------------

filterCamTrap <- function(x, col, 
                             string = "PIÈGE PHOTO|PIÈGE-PHOTO|PIEGE PHOTO|PIEGE PHOTO"){
  x %>%
    filter(!grepl(string, {{ col }}))
}

### Get relevant columns -------------------------------------------------------

formatData <- function(x,
                       dataSourceStr,
                       observerCol,
                       protocolCol,
                       dateCol,
                       presenceCond,
                       xCol = NA,
                       yCol = NA,
                       gridCellCol = NA,
                       dateformat) {
  x <- x %>%
    mutate(
      dataSource = {{ dataSourceStr }},
      observer = {{ observerCol }},
      protocol = {{ protocolCol }},
      date = as.Date({{ dateCol }}, format = dateformat),
      year = year(date),
      presence = as.numeric({{ presenceCond }}),
      lon = {{ xCol }},
      lat = {{ yCol }},
      gridCell = {{ gridCellCol }}
    )
  if (all(is.na(x$gridCell))) {
    x <- x %>%
      mutate(gridCell = ifelse(
        lon >= 1000000,
        paste0("E", substr(lon, 1, 3), "N", substr(lat, 1, 3)),
        paste0("E0", substr(lon, 1, 2), "N", substr(lat, 1, 3))
      ))
  }
  x %>%
    select(dataSource, observer, protocol, date, year, presence, lon, lat, gridCell)
}





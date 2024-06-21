library(tidyverse)
library(mvtnorm)
library(stars)
library(sf)
library(purrr)
library(foreach)

rm(list = ls())

source("src/functions/simulate_fcts.r")

years <- 1:5

grid.filename <- "data/L9310x10grid.rds"

grid <- readRDS(grid.filename) %>%
  st_as_sf(crs = 2154)

mu <- list(
  c(0.15,0.7),
  c(0.55,0.4),
  c(0.3,0.3))

sigma <-list(
  c(5000, 2500),
  c(5000, 1000),
  c(2500, 5000))

r <- 2500

simDat <- map(years, grid, .f = function(t, grid){
  grid$year <- t
  grid$lambda <- map(1:3, .f = function(i){
    gen_mvn(grid, mu[[i]], sigma[[i]] + c(r,r) * (t-1), 0)}) %>%
    reduce(`+`)
    
  grid
}) 

simDat <- reduce(simDat, rbind)

ggplot(simDat) + 
  geom_sf(aes(fill =  lambda), col = NA) +
  facet_wrap(~year) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_bw()

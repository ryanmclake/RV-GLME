# Clear your environment and memory
rm(list=ls())
gc()

# install packages needed for the analysis

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(ggplot2, tidyverse, lubridate, readr, reshape2, patchwork,
               scales, maps, rnaturalearth, sf, nlstools, minpack.lm, 
               rnaturalearthdata, rnaturalearthhires)




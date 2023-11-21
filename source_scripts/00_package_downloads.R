# Clear your environment and memory
rm(list=ls())
gc()

# install packages needed for the analysis

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(ggplot2, tidyverse, lubridate, readr, reshape2, zoo, patchwork,
               scales, gridExtra, maps, hexbin, rnaturalearth, sf, 
               nlstools, minpack.lm, imputeTS, hydroGOF, devtools, vroom)
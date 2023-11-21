# Clear your environment and memory
rm(list=ls())
gc()

# install packages needed for the analysis

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(ggplot2, tidyverse, lubridate, readr, reshape2, zoo, patchwork,
               scales, gridExtra, maps, hexbin, rnaturalearth, sf, 
               nlstools, minpack.lm, imputeTS, hydroGOF, devtools, vroom)

# Ryan, the following packages wouldn't load for me 
# Failed to install/load:
#  maps, rnaturalearth, sf, hydroGOF

# I was able to install all but hydroGOF manually:
# Warning in install.packages :
# package ‘hydroGOF’ is not available for this version of R

# A version of this package for your version of R might be available elsewhere,
# see the ideas at
# https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

# Maybe we need to specify a version of R
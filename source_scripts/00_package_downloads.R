# Clear your environment and memory
rm(list=ls())
gc()

# install packages needed for the analysis

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(ggplot2, tidyverse, lubridate, readr, reshape2, zoo, patchwork,
               scales, gridExtra, maps, hexbin, rnaturalearth, sf, 
               nlstools, minpack.lm, imputeTS, hydroGOF, devtools, vroom,
               rnaturalearthdata)

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

# but then when I went to load the packages, for sf and rnaturalearth I got
# the following:
# but this won't load
# Error: package or namespace load failed for ‘rnaturalearth’ in dyn.load(file, DLLpath = DLLpath, ...):
#   unable to load shared object '/usr/local/lib/R/site-library/units/libs/units.so':
#   libudunits2.so.0: cannot open shared object file: No such file or directory

# so now I am switching over to my laptop b/c I think this is a Docker problem
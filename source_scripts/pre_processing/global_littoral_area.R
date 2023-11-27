### This is a script that pulls individual .tif bathymetry files from https://www.nature.com/articles/s41597-022-01132-9
### and calculates the area of the lake with depths "LESS THAN/SHALLOWER THAN" 3 meters. A proxy for ebullition emissions following Bastviken et al.,2004 
### NOTE - this code can be applied to test any depth. So if you want to explore a photoc zone ~8.5 meters, then you can calculate that across all HydroLakes. 

# Clear your environment and memory
rm(list=ls())
gc()


library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stars, warn.conflicts = FALSE)
library(units, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)
library(utils, warn.conflicts = FALSE)

bathy <- c(list.files("/Volumes/SeagateBackupPlusDrive/Bathymetry_Rasters/", pattern='.tif', recursive = T))

extract_littoral <- function(bathy){
  
  whole <- stars::read_stars(paste0("/Volumes/SeagateBackupPlusDrive/Bathymetry_Rasters/",bathy,"")) %>%
    sf::st_as_sf(.) %>% 
    dplyr::rename_at(1, ~'depth_m') %>%
    dplyr::mutate(lake_area_m2 = st_area(geometry)) %>%
    sf::st_drop_geometry(.) %>% 
    units::drop_units(.) %>%
    dplyr::summarise(lake_area_km2 = sum(lake_area_m2)/1000) %>%
    dplyr::mutate(hylak_id = sub(".*/", "", bathy),
                  hylak_id = stringr::str_extract(hylak_id, "[^_]*"))
  
  lit <- stars::read_stars(paste0("/Volumes/SeagateBackupPlusDrive/Bathymetry_Rasters/",bathy,"")) %>%
    sf::st_as_sf(.) %>% 
    dplyr::rename_at(1, ~'depth_m') %>%
    dplyr::mutate(max_depth_m = max(depth_m, na.rm = T)) %>%
    dplyr::mutate(mean_depth_m = mean(depth_m, na.rm = T)) %>%
    dplyr::filter(depth_m <= 3) %>%
    dplyr::mutate(littoral_area_m2 = st_area(geometry)) %>% 
    sf::st_drop_geometry(.) %>% 
    units::drop_units(.) %>%
    dplyr::summarise(mean_littoral_depth_m = mean(depth_m),
                     max_depth_m = mean(max_depth_m),
                     mean_depth_m = mean(mean_depth_m),
                     littoral_area_km2 = sum(littoral_area_m2)/1000) %>%
    dplyr::mutate(hylak_id = sub(".*/", "", bathy),
                  hylak_id = stringr::str_extract(hylak_id, "[^_]*")) %>%
    dplyr::left_join(., whole, by = "hylak_id") %>%
    dplyr::mutate(littoral_fraction = (littoral_area_km2/lake_area_km2)) %>%
    dplyr::select(hylak_id, max_depth_m, mean_depth_m, littoral_fraction) %>%
    utils::write.table(., file = paste0("./output_data/global_littoral_area.csv"),
                       append = T,
                       row.names = F,
                       col.names = !file.exists("./output_data/global_littoral_area.csv"))
  
  return(unique(bathy))
}


s = Sys.time()

no_cores <- detectCores()-2
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=bathy) %dopar% extract_littoral(i)

e <- Sys.time()
t=e-s
print(t)

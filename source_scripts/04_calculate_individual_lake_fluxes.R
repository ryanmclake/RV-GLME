# Clear your environment and memory
rm(list=ls())
gc()

# Source the functions from the past script
source("./source_scripts/03_fit_equation_scenarios.R")

# Prepare the GLCP data to apply the functions to
# read in glcp for Belgium
dat <- vroom::vroom("./source_data/Belgium.csv", col_names = F) %>%
  # rename columns since it was adapted in terminal
  rename(year = X1, month = X2, hylak_id = X3, lat = X4, lon = X5, continent = X6, 
         country = X7, seasonal_area_km2 = X8, lake_type = X9, mean_temp_k = X10, ice_cover_mean = X11) %>%
  # clean up some of the spurious ice data
  mutate(ice_cover_mean = ifelse(is.na(ice_cover_mean),0,ice_cover_mean))

# Calculate diffusion
diff <- dat %>%
  group_by(hylak_id, lat, lon, seasonal_area_km2) %>%
  # Apply the functions
  mutate(BASELINE_diffusion = ifelse(lake_type == 1, mapply(BASELINE_lake_diff, x=mean_temp_k-273.15), mapply(BASELINE_res_diff, x=mean_temp_k-273.15)),
         TIME_LOW_diffusion = ifelse(lake_type == 1, mapply(TIME_LOW_lake_diff, x=mean_temp_k-273.15), mapply(TIME_LOW_res_diff, x=mean_temp_k-273.15)),
         TIME_HIGH_diffusion = ifelse(lake_type == 1, mapply(TIME_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(TIME_HIGH_res_diff, x=mean_temp_k-273.15)),
         SPACE_LOW_diffusion = ifelse(lake_type == 1, mapply(SPACE_LOW_lake_diff, x=mean_temp_k-273.15), mapply(SPACE_LOW_res_diff, x=mean_temp_k-273.15)),
         SPACE_HIGH_diffusion = ifelse(lake_type == 1, mapply(SPACE_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(SPACE_HIGH_res_diff, x=mean_temp_k-273.15)),
         COEFF_LOW_diffusion = ifelse(lake_type == 1, mapply(COEFF_LOW_lake_diff, x=mean_temp_k-273.15), mapply(COEFF_LOW_res_diff, x=mean_temp_k-273.15)),
         COEFF_HIGH_diffusion = ifelse(lake_type == 1, mapply(COEFF_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(COEFF_HIGH_res_diff, x=mean_temp_k-273.15)),
         MODEL_LOW_diffusion = ifelse(lake_type == 1, mapply(MODEL_LOW_lake_diff, x=mean_temp_k-273.15), mapply(MODEL_LOW_res_diff, x=mean_temp_k-273.15)),
         MODEL_HIGH_diffusion = ifelse(lake_type == 1, mapply(MODEL_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(MODEL_HIGH_res_diff, x=mean_temp_k-273.15)),
         # if the ice cover is over 60%, cut off fluxes to 0
         # we may need to clean this up according to MFM and YY
         BASELINE_diffusion = ifelse(ice_cover_mean > 60, 0, BASELINE_diffusion),
         TIME_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, TIME_LOW_diffusion),
         TIME_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, TIME_HIGH_diffusion),
         SPACE_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, SPACE_LOW_diffusion),
         SPACE_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, SPACE_HIGH_diffusion),
         COEFF_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, COEFF_LOW_diffusion),
         COEFF_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, COEFF_HIGH_diffusion),
         MODEL_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, MODEL_LOW_diffusion),
         MODEL_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, MODEL_HIGH_diffusion),
         # although there are basically no emissions below zero, However, this clears this
         # up in case this happens. There are always some spurious data since this 
         # data product is like 300+ million rows
         BASELINE_diffusion = ifelse(BASELINE_diffusion < 0, 0, BASELINE_diffusion),
         TIME_LOW_diffusion = ifelse(TIME_LOW_diffusion < 0, 0, TIME_LOW_diffusion),
         TIME_HIGH_diffusion = ifelse(TIME_HIGH_diffusion < 0, 0, TIME_HIGH_diffusion),
         SPACE_LOW_diffusion = ifelse(SPACE_LOW_diffusion < 0, 0, SPACE_LOW_diffusion),
         SPACE_HIGH_diffusion = ifelse(SPACE_HIGH_diffusion < 0, 0, SPACE_HIGH_diffusion),
         COEFF_LOW_diffusion = ifelse(COEFF_LOW_diffusion < 0, 0, COEFF_LOW_diffusion),
         COEFF_HIGH_diffusion = ifelse(COEFF_HIGH_diffusion < 0, 0, COEFF_HIGH_diffusion),
         MODEL_LOW_diffusion = ifelse(MODEL_LOW_diffusion < 0, 0, MODEL_LOW_diffusion),
         MODEL_HIGH_diffusion = ifelse(MODEL_HIGH_diffusion < 0, 0, MODEL_HIGH_diffusion),
         # turn the year and month into a date format 
         'date' = lubridate::make_date(year = year, month = month)) %>%
  ungroup(.) %>%
  select(-year, -month) %>%
  # save the diffusion predictions
  readr::write_csv(., "./output_data/belgium_diffusion_prediction.csv")

# Correct fluxes to match latest papers and scale to mgCH4 m-2 yr-1
d <- vroom::vroom("./output_data/belgium_diffusion_prediction.csv") %>%
 dplyr::group_by(hylak_id, lat, lon) %>%
 dplyr::summarise(`Baseline Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
          `Time-Low Estimate` = sum(TIME_LOW_diffusion)/sum(seasonal_area_km2),
          `Time-high Estimate` = sum(TIME_HIGH_diffusion)/sum(seasonal_area_km2),
          `Space-Low Estimate` = sum(SPACE_LOW_diffusion)/sum(seasonal_area_km2),
          `Space-High Estimate` = sum(SPACE_HIGH_diffusion)/sum(seasonal_area_km2),
          `Param-Low Estimate` = sum(COEFF_LOW_diffusion)/sum(seasonal_area_km2),
          `Param-High Estimate` = sum(COEFF_HIGH_diffusion)/sum(seasonal_area_km2),
          `Model-Low Estimate` = sum(MODEL_LOW_diffusion)/sum(seasonal_area_km2),
          `Model-High Estimate` = sum(MODEL_HIGH_diffusion)/sum(seasonal_area_km2))%>%
 mutate(`Baseline Estimate` = `Baseline Estimate` * 0.001 * 365,
     `Time-Low Estimate` = `Time-Low Estimate` * 0.001 * 365,
     `Time-high Estimate` = `Time-high Estimate` * 0.001 * 365,
     `Space-Low Estimate` = `Space-Low Estimate` * 0.001 * 365,
     `Space-High Estimate` = `Space-High Estimate` * 0.001 * 365,
     `Param-Low Estimate` = `Param-Low Estimate` * 0.001 * 365,
     `Param-High Estimate` = `Param-High Estimate` * 0.001 * 365,
     `Model-Low Estimate` = `Model-Low Estimate` * 0.001 * 365,
     `Model-High Estimate` = `Model-High Estimate` * 0.001 * 365)


# Calculate ebullition
# Annotations are same as above
ebu <- dat %>%
  group_by(hylak_id, lat, lon, seasonal_area_km2) %>%
  mutate(BASELINE_ebullition = ifelse(lake_type == 1, mapply(BASELINE_lake_ebu, x=mean_temp_k-273.15), mapply(BASELINE_res_ebu, x=mean_temp_k-273.15)),
         TIME_LOW_ebullition = ifelse(lake_type == 1, mapply(TIME_LOW_lake_ebu, x=mean_temp_k-273.15), mapply(TIME_LOW_res_ebu, x=mean_temp_k-273.15)),
         TIME_HIGH_ebullition = ifelse(lake_type == 1, mapply(TIME_HIGH_lake_ebu, x=mean_temp_k-273.15), mapply(TIME_HIGH_res_ebu, x=mean_temp_k-273.15)),
         SPACE_LOW_ebullition = ifelse(lake_type == 1, mapply(SPACE_LOW_lake_ebu, x=mean_temp_k-273.15), mapply(SPACE_LOW_res_ebu, x=mean_temp_k-273.15)),
         SPACE_HIGH_ebullition = ifelse(lake_type == 1, mapply(SPACE_HIGH_lake_ebu, x=mean_temp_k-273.15), mapply(SPACE_HIGH_res_ebu, x=mean_temp_k-273.15)),
         COEFF_LOW_ebullition = ifelse(lake_type == 1, mapply(COEFF_LOW_lake_ebu, x=mean_temp_k-273.15), mapply(COEFF_LOW_res_ebu, x=mean_temp_k-273.15)),
         COEFF_HIGH_ebullition = ifelse(lake_type == 1, mapply(COEFF_HIGH_lake_ebu, x=mean_temp_k-273.15), mapply(COEFF_HIGH_res_ebu, x=mean_temp_k-273.15)),
         MODEL_LOW_ebullition = ifelse(lake_type == 1, mapply(MODEL_LOW_lake_ebu, x=mean_temp_k-273.15), mapply(MODEL_LOW_res_ebu, x=mean_temp_k-273.15)),
         MODEL_HIGH_ebullition = ifelse(lake_type == 1, mapply(MODEL_HIGH_lake_ebu, x=mean_temp_k-273.15), mapply(MODEL_HIGH_res_ebu, x=mean_temp_k-273.15)),
         BASELINE_ebullition = ifelse(ice_cover_mean > 60, 0, BASELINE_ebullition),
         TIME_LOW_ebullition = ifelse(ice_cover_mean > 60, 0, TIME_LOW_ebullition),
         TIME_HIGH_ebullition = ifelse(ice_cover_mean > 60, 0, TIME_HIGH_ebullition),
         SPACE_LOW_ebullition = ifelse(ice_cover_mean > 60, 0, SPACE_LOW_ebullition),
         SPACE_HIGH_ebullition = ifelse(ice_cover_mean > 60, 0, SPACE_HIGH_ebullition),
         COEFF_LOW_ebullition = ifelse(ice_cover_mean > 60, 0, COEFF_LOW_ebullition),
         COEFF_HIGH_ebullition = ifelse(ice_cover_mean > 60, 0, COEFF_HIGH_ebullition),
         MODEL_LOW_ebullition = ifelse(ice_cover_mean > 60, 0, MODEL_LOW_ebullition),
         MODEL_HIGH_ebullition = ifelse(ice_cover_mean > 60, 0, MODEL_HIGH_ebullition),
         BASELINE_ebullition = ifelse(BASELINE_ebullition < 0, 0, BASELINE_ebullition),
         TIME_LOW_ebullition = ifelse(TIME_LOW_ebullition < 0, 0, TIME_LOW_ebullition),
         TIME_HIGH_ebullition = ifelse(TIME_HIGH_ebullition < 0, 0, TIME_HIGH_ebullition),
         SPACE_LOW_ebullition = ifelse(SPACE_LOW_ebullition < 0, 0, SPACE_LOW_ebullition),
         SPACE_HIGH_ebullition = ifelse(SPACE_HIGH_ebullition < 0, 0, SPACE_HIGH_ebullition),
         COEFF_LOW_ebullition = ifelse(COEFF_LOW_ebullition < 0, 0, COEFF_LOW_ebullition),
         COEFF_HIGH_ebullition = ifelse(COEFF_HIGH_ebullition < 0, 0, COEFF_HIGH_ebullition),
         MODEL_LOW_ebullition = ifelse(MODEL_LOW_ebullition < 0, 0, MODEL_LOW_ebullition),
         MODEL_HIGH_ebullition = ifelse(MODEL_HIGH_ebullition < 0, 0, MODEL_HIGH_ebullition),
         'date' = lubridate::make_date(year = year, month = month)) %>%
  ungroup(.) %>%
  select(-year, -month) %>%
  readr::write_csv(., "./output_data/belgium_ebullition_prediction.csv")

# Correct ebullition fluxes to match latest papers and scale to mgCH4 m-2 yr-1
# read in littoral area percentage of lake shallower than 3 meters
# The code to make this data is cool and will be described briefly in a supplement
# basically, ebullition cannot occur across a lake because of depth
# this determines what percentage of the lake is <3m (according to Bastvike threshold)
# and then multiplies it by that fraction to correct so ebullition is not assumed to occur 
# across the whole waterbody

# read in derived data product from code
lit <- read_csv("./source_data/global_littoral_area.csv")

e <- vroom::vroom("./output_data/belgium_ebullition_prediction.csv") %>%
  dplyr::group_by(hylak_id, lat, lon) %>%
  dplyr::summarise(`Baseline Estimate` = sum(BASELINE_ebullition)/sum(seasonal_area_km2),
                   `Time-Low Estimate` = sum(TIME_LOW_ebullition)/sum(seasonal_area_km2),
                   `Time-high Estimate` = sum(TIME_HIGH_ebullition)/sum(seasonal_area_km2),
                   `Space-Low Estimate` = sum(SPACE_LOW_ebullition)/sum(seasonal_area_km2),
                   `Space-High Estimate` = sum(SPACE_HIGH_ebullition)/sum(seasonal_area_km2),
                   `Param-Low Estimate` = sum(COEFF_LOW_ebullition)/sum(seasonal_area_km2),
                   `Param-High Estimate` = sum(COEFF_HIGH_ebullition)/sum(seasonal_area_km2),
                   `Model-Low Estimate` = sum(MODEL_LOW_ebullition)/sum(seasonal_area_km2),
                   `Model-High Estimate` = sum(MODEL_HIGH_ebullition)/sum(seasonal_area_km2)) %>%
 left_join(., lit, by = "hylak_id") %>%
  mutate(`Baseline Estimate` = `Baseline Estimate` * 0.001 * 365 * littoral_fraction,
     `Time-Low Estimate` = `Time-Low Estimate` * 0.001 * 365 * littoral_fraction,
     `Time-high Estimate` = `Time-high Estimate` * 0.001 * 365 * littoral_fraction,
     `Space-Low Estimate` = `Space-Low Estimate` * 0.001 * 365 * littoral_fraction,
     `Space-High Estimate` = `Space-High Estimate` * 0.001 * 365 * littoral_fraction,
     `Param-Low Estimate` = `Param-Low Estimate` * 0.001 * 365 * littoral_fraction,
     `Param-High Estimate` = `Param-High Estimate` * 0.001 * 365 * littoral_fraction,
     `Model-Low Estimate` = `Model-Low Estimate` * 0.001 * 365 * littoral_fraction,
     `Model-High Estimate` = `Model-High Estimate` * 0.001 * 365 * littoral_fraction)

# combine the corrected fluxes to make maps in the next script
COMBINED_FLUXES <- bind_rows(e,d) %>%
  select(-max_depth_m, -mean_depth_m, -littoral_fraction) %>%
  group_by(hylak_id, lat, lon) %>%
  summarise_all(funs(sum)) %>%
  write_csv(., "./output_data/belgium_combined_and_adjusted_prediction.csv")
  





# Clear your environment and memory
rm(list=ls())
gc()

source("./source_scripts/03_fit_equation_scenarios.R")

dat <- vroom::vroom("./source_data/japan_data_from_glcp.csv")

diff <- dat %>%
  group_by(hylak_id, lat, lon, seasonal_area_km2) %>%
  mutate(BASELINE_diffusion = ifelse(lake_type == 1, mapply(BASELINE_lake_diff, x=mean_temp_k-273.15), mapply(BASELINE_res_diff, x=mean_temp_k-273.15)),
         TIME_LOW_diffusion = ifelse(lake_type == 1, mapply(TIME_LOW_lake_diff, x=mean_temp_k-273.15), mapply(TIME_LOW_res_diff, x=mean_temp_k-273.15)),
         TIME_HIGH_diffusion = ifelse(lake_type == 1, mapply(TIME_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(TIME_HIGH_res_diff, x=mean_temp_k-273.15)),
         SPACE_LOW_diffusion = ifelse(lake_type == 1, mapply(SPACE_LOW_lake_diff, x=mean_temp_k-273.15), mapply(SPACE_LOW_res_diff, x=mean_temp_k-273.15)),
         SPACE_HIGH_diffusion = ifelse(lake_type == 1, mapply(SPACE_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(SPACE_HIGH_res_diff, x=mean_temp_k-273.15)),
         COEFF_LOW_diffusion = ifelse(lake_type == 1, mapply(COEFF_LOW_lake_diff, x=mean_temp_k-273.15), mapply(COEFF_LOW_res_diff, x=mean_temp_k-273.15)),
         COEFF_HIGH_diffusion = ifelse(lake_type == 1, mapply(COEFF_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(COEFF_HIGH_res_diff, x=mean_temp_k-273.15)),
         MODEL_LOW_diffusion = ifelse(lake_type == 1, mapply(MODEL_LOW_lake_diff, x=mean_temp_k-273.15), mapply(MODEL_LOW_res_diff, x=mean_temp_k-273.15)),
         MODEL_HIGH_diffusion = ifelse(lake_type == 1, mapply(MODEL_HIGH_lake_diff, x=mean_temp_k-273.15), mapply(MODEL_HIGH_res_diff, x=mean_temp_k-273.15)),
         BASELINE_diffusion = ifelse(ice_cover_mean > 60, 0, BASELINE_diffusion),
         TIME_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, TIME_LOW_diffusion),
         TIME_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, TIME_HIGH_diffusion),
         SPACE_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, SPACE_LOW_diffusion),
         SPACE_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, SPACE_HIGH_diffusion),
         COEFF_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, COEFF_LOW_diffusion),
         COEFF_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, COEFF_HIGH_diffusion),
         MODEL_LOW_diffusion = ifelse(ice_cover_mean > 60, 0, MODEL_LOW_diffusion),
         MODEL_HIGH_diffusion = ifelse(ice_cover_mean > 60, 0, MODEL_HIGH_diffusion),
         BASELINE_diffusion = ifelse(BASELINE_diffusion < 0, 0, BASELINE_diffusion),
         TIME_LOW_diffusion = ifelse(TIME_LOW_diffusion < 0, 0, TIME_LOW_diffusion),
         TIME_HIGH_diffusion = ifelse(TIME_HIGH_diffusion < 0, 0, TIME_HIGH_diffusion),
         SPACE_LOW_diffusion = ifelse(SPACE_LOW_diffusion < 0, 0, SPACE_LOW_diffusion),
         SPACE_HIGH_diffusion = ifelse(SPACE_HIGH_diffusion < 0, 0, SPACE_HIGH_diffusion),
         COEFF_LOW_diffusion = ifelse(COEFF_LOW_diffusion < 0, 0, COEFF_LOW_diffusion),
         COEFF_HIGH_diffusion = ifelse(COEFF_HIGH_diffusion < 0, 0, COEFF_HIGH_diffusion),
         MODEL_LOW_diffusion = ifelse(MODEL_LOW_diffusion < 0, 0, MODEL_LOW_diffusion),
         MODEL_HIGH_diffusion = ifelse(MODEL_HIGH_diffusion < 0, 0, MODEL_HIGH_diffusion),
         'date' = lubridate::make_date(year = year, month = month)) %>%
  ungroup(.) %>%
  select(-year, -month) %>%
  readr::write_csv(., "./output_data/japan_diffusion_prediction.csv")


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
  readr::write_csv(., "./output_data/japan_ebullition_prediction.csv")


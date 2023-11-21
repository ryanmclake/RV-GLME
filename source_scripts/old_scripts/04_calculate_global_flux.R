# Clear your environment and memory
rm(list=ls())
gc()

source("./source_scripts/03_fit_equation_scenarios.R")

dat <- vroom::vroom("./source_data/Belgium.csv", col_names = F) %>%
  rename(year = X1, month = X2, hylak_id = X3, lat = X4, lon = X5, continent = X6, 
         country = X7, seasonal_area_km2 = X8, lake_type = X9, mean_temp_k = X10, ice_cover_mean = X11) %>%
  mutate(ice_cover_mean = ifelse(is.na(ice_cover_mean),0,ice_cover_mean))

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
         'date' = lubridate::make_date(year = year, month = month)) %>%
  ungroup(.) %>%
  select(-year, -month) %>%
  readr::write_csv(., "./output_data/belgium_diffusion_prediction.csv")


d <- vroom::vroom("./output_data/belgium_diffusion_prediction.csv") %>%
  dplyr::group_by(hylak_id, lat, lon) %>%
  dplyr::summarise(`Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Ebullition Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2),
                   `Baseline Diffusion Estimate` = sum(BASELINE_diffusion)/sum(seasonal_area_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_BASE_sum.csv")


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











vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_BASE.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Baseline Ebullition Predictions` = sum(FOA_mean_ebullition)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_BASE_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_TIME.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Time-low Ebu Predictions` = sum(FOA_time_low_ebu)/sum(total_km2),
                   `Time-high Ebu Predictions` = sum(FOA_time_high_ebu)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_TIME_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_SPACE.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Space-low Ebu Predictions` = sum(FOA_space_low_ebu)/sum(total_km2),
                   `Space-high Ebu Predictions` = sum(FOA_space_high_ebu)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_SPACE_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_MODEL.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Model-low Ebu Predictions` = sum(FOA_mean_error_low_ebu)/sum(total_km2),
                   `Model-high Ebu Predictions` = sum(FOA_mean_error_high_ebu)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_MODEL_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_PARAM.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Param-low Ebu Predictions` = sum(FOA_coefficient_low_ebu)/sum(total_km2),
                   `Param-high Ebu Predictions` = sum(FOA_coefficient_high_ebu)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_ebullition_variability_PARAM_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_BASE.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Baseline Diffusion Predictions` = sum(FOA_mean_diffusion)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_BASE_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_TIME.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Time-low Diff Predictions` = sum(FOA_time_low_diff)/sum(total_km2),
                   `Time-high Diff Predictions` = sum(FOA_time_high_diff)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_TIME_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_SPACE.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Space-low Diff Predictions` = sum(FOA_space_low_diff)/sum(total_km2),
                   `Space-high Diff Predictions` = sum(FOA_space_high_diff)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_SPACE_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_MODEL.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Model-low Diff Predictions` = sum(FOA_mean_error_low_diff)/sum(total_km2),
                   `Model-high Diff Predictions` = sum(FOA_mean_error_high_diff)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_MODEL_sum.csv")

vroom::vroom("/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_PARAM.csv") %>%
  dplyr::group_by(centr_lat, centr_lon) %>%
  dplyr::summarise(`Param-low Diff Predictions` = sum(FOA_coefficient_low_diff)/sum(total_km2),
                   `Param-high Diff Predictions` = sum(FOA_coefficient_high_diff)/sum(total_km2)) %>%
  readr::write_csv(., "/central/groups/carnegie_poc/rmcclure/project-GLEE/output/Global_diffusion_variability_PARAM_sum.csv")


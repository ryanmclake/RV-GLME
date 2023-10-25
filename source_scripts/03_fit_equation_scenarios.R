# Clear your environment and memory
rm(list=ls())
gc()

set.seed(1098)

d <- readr::read_csv("./output_data/GLEE_data_for_model_fitting.csv") %>%
  mutate(temp_for_model_C = temp_for_model_K-273.15)

# ASSIGN THE DIFFERENT DATA FOR THE RESPECTIVE CALIBRATIONS

# lake diffusion data
lake_diff <- d %>% filter(waterbody_type == "lake") %>%
  select(ch4_diff,ch4_diff_time_low,ch4_diff_time_high,ch4_diff_space_low,ch4_diff_space_high,temp_for_model_C) %>%
  na.omit(.)

# lake ebullition data
lake_ebu <- d %>% filter(waterbody_type == "lake")%>%
  select(ch4_ebu,ch4_ebu_time_low,ch4_ebu_time_high,ch4_ebu_space_low,ch4_ebu_space_high,temp_for_model_C) %>%
  na.omit(.)

# reservoir diffusion
res_diff <- d %>% filter(waterbody_type == "reservoir") %>%
  select(ch4_diff,ch4_diff_time_low,ch4_diff_time_high,ch4_diff_space_low,ch4_diff_space_high,temp_for_model_C) %>%
  na.omit(.)

# reservoir ebullition
res_ebu <- d %>% filter(waterbody_type == "reservoir")%>%
  select(ch4_ebu,ch4_ebu_time_low,ch4_ebu_time_high,ch4_ebu_space_low,ch4_ebu_space_high,temp_for_model_C) %>%
  na.omit(.)

# FIT ALL 36 VARIABILITY SCENARIOS

### BASELINE MODEL FITS ###
# eqn 1 --> Baseline Lake Diffusion
lake_diff_base = nlsLM(ch4_diff ~ A * a^(temp_for_model_C-20),
                      start = list(A = 100, a = 1.1),
                      data = lake_diff,
                      control = nls.lm.control(maxiter=1000))

BASELINE_lake_diff <- function(x){
  predicted_diff_rate = 23.631 * 1.081^(x-20)
  return(predicted_diff_rate)
}

# eqn 2 --> Baseline Lake Ebullition
lake_ebu_base = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                       start = list(A = 100, a = 1.1),
                       data = lake_ebu,
                       control = nls.lm.control(maxiter=1000))

BASELINE_lake_ebu <- function(x){
  predicted_ebu_rate = 79.741 * 1.042^(x-20)
  return(predicted_ebu_rate)
}

# eqn 3 --> Baseline Reservoir Diffusion
res_diff_base = nlsLM(ch4_diff ~ A * a^(temp_for_model_C-20),
                       start = list(A = 100, a = 1.1),
                       data = res_diff,
                       control = nls.lm.control(maxiter=1000))

BASELINE_res_diff <- function(x){
  predicted_diff_rate = 25.223 * 1.026^(x-20)
  return(predicted_diff_rate)
}

# eqn 4 --> Baseline Reservoir Ebullition
res_ebu_base = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                      start = list(A = 100, a = 1.1),
                      data = res_ebu,
                      control = nls.lm.control(maxiter=1000))

BASELINE_res_ebu <- function(x){
  predicted_ebu_rate = 58.530 * 1.428^(x-20)
  return(predicted_ebu_rate)
}

### MEASUREMENT TEMPORAL VARIABILITY MODEL FITS ###

# eqn 5 --> Temporal Low Lake Diffusion
lake_diff_temp_low = nlsLM(ch4_diff_time_low ~ A * a^(temp_for_model_C-20),
                       start = list(A = 100, a = 1.1),
                       data = lake_diff,
                       control = nls.lm.control(maxiter=1000))

TIME_LOW_lake_diff <- function(x){
  predicted_diff_rate = 0.1095 * 1.0305^(x-20)
  return(predicted_diff_rate)
}

# eqn 6 --> Temporal High Lake Diffusion
lake_diff_temp_high = nlsLM(ch4_diff_time_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = lake_diff,
                           control = nls.lm.control(maxiter=1000))

TIME_HIGH_lake_diff <- function(x){
  predicted_diff_rate = 4360.0360 * 0.9912^(x-20)
  return(predicted_diff_rate)
}

# eqn 7 --> Temporal low Lake Ebullition
lake_ebu_temp_low = nlsLM(ch4_ebu_time_low ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = lake_ebu,
                            control = nls.lm.control(maxiter=1000))

TIME_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = 0.0467 * 0.9661^(x-20)
  return(predicted_ebu_rate)
}

# eqn 8 --> Temporal High Lake Ebullition
lake_ebu_temp_high = nlsLM(ch4_ebu_time_high ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = lake_ebu,
                            control = nls.lm.control(maxiter=1000))

TIME_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = 1870.045 * 1.034^(x-20)
  return(predicted_ebu_rate)
}

# eqn 9 --> Temporal Low Reservoir Diffusion
res_diff_temp_low = nlsLM(ch4_diff_time_low ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = res_diff,
                           control = nls.lm.control(maxiter=1000))

TIME_LOW_res_diff <- function(x){
  predicted_diff_rate = 0.002339 * 0.973227^(x-20)
  return(predicted_diff_rate)
}

# eqn 10 --> Temporal High Reservoir Diffusion
res_diff_temp_high = nlsLM(ch4_diff_time_high ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = res_diff,
                            control = nls.lm.control(maxiter=1000))

TIME_HIGH_res_diff <- function(x){
  predicted_diff_rate = 5227.842 * 1.001^(x-20)
  return(predicted_diff_rate)
}

# eqn 11 --> Temporal Low Reservoir Ebullition
res_ebu_temp_low = nlsLM(ch4_ebu_time_low ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = res_ebu,
                          control = nls.lm.control(maxiter=1000))

TIME_LOW_res_ebu <- function(x){
  predicted_ebu_rate = 0.02235 * 0.90280^(x-20)
  return(predicted_ebu_rate)
}

# eqn 12 --> Temporal High Reservoir Ebullition
res_ebu_temp_high = nlsLM(ch4_ebu_time_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = res_ebu,
                           control = nls.lm.control(maxiter=1000))

TIME_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = 2267.88 * 1.01^(x-20)
  return(predicted_ebu_rate)
}


### MEASUREMENT SPATIAL VARIABILITY MODEL FITS ###

# eqn 13 --> Spatial Low Lake Diffusion
lake_diff_space_low = nlsLM(ch4_diff_space_low ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = lake_diff,
                           control = nls.lm.control(maxiter=1000))

SPACE_LOW_lake_diff <- function(x){
  predicted_diff_rate = 0.01001 * 1.02867^(x-20)
  return(predicted_diff_rate)
}

# eqn 14 --> Spatial High Lake Diffusion
lake_diff_space_high = nlsLM(ch4_diff_space_high ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = lake_diff,
                            control = nls.lm.control(maxiter=1000))

SPACE_HIGH_lake_diff <- function(x){
  predicted_diff_rate = 3960.4486 * 0.9878^(x-20)
  return(predicted_diff_rate)
}

# eqn 15 --> Spatial low Lake Ebullition
lake_ebu_space_low = nlsLM(ch4_ebu_space_low ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = lake_ebu,
                          control = nls.lm.control(maxiter=1000))

SPACE_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = 0.07087 * 1.01325^(x-20)
  return(predicted_ebu_rate)
}

# eqn 16 --> Spatial High Lake Ebullition
lake_ebu_space_high = nlsLM(ch4_ebu_space_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = lake_ebu,
                           control = nls.lm.control(maxiter=1000))

SPACE_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = 1483.1033 * 0.9857^(x-20)
  return(predicted_ebu_rate)
}

# eqn 17 --> Spatial Low Reservoir Diffusion
res_diff_space_low = nlsLM(ch4_diff_space_low ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = res_diff,
                          control = nls.lm.control(maxiter=1000))

SPACE_LOW_res_diff <- function(x){
  predicted_diff_rate = 0.01374 * 1.13689^(x-20)
  return(predicted_diff_rate)
}

# eqn 18 --> Spatial High Reservoir Diffusion
res_diff_space_high = nlsLM(ch4_diff_space_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = res_diff,
                           control = nls.lm.control(maxiter=1000))

SPACE_HIGH_res_diff <- function(x){
  predicted_diff_rate = 3721.2281 * 0.9723^(x-20)
  return(predicted_diff_rate)
}


# eqn 19 --> Spatial Low Reservoir Ebullition
res_ebu_space_low = nlsLM(ch4_ebu_space_low ~ A * a^(temp_for_model_C-20),
                         start = list(A = 100, a = 1.1),
                         data = res_ebu,
                         control = nls.lm.control(maxiter=1000))

SPACE_LOW_res_ebu <- function(x){
  predicted_ebu_rate = 0.01689 * 0.99834^(x-20)
  return(predicted_ebu_rate)
}

# eqn 20 --> Spatial High Reservoir Ebullition
res_ebu_space_high = nlsLM(ch4_ebu_space_high ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = res_ebu,
                          control = nls.lm.control(maxiter=1000))

SPACE_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = 2263.558 * 1.003^(x-20)
  return(predicted_ebu_rate)
}


### COEFFICIENT VARIABILITY MODEL FITS ###
# eqn 21 & 22 --> COEFFICIENT Lake Diffusion
summary(lake_diff_base)

COEFF_LOW_lake_diff <- function(x){
  predicted_diff_rate = 19.78323 * 1.06086^(x-20)
  return(predicted_diff_rate)
}

COEFF_HIGH_lake_diff <- function(x){
  predicted_diff_rate = 27.47969 * 1.10068^(x-20)
  return(predicted_diff_rate)
}

# eqn 23 & 24 --> Parameter Lake Ebullition
summary(lake_ebu_base)

COEFF_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = 69.65971 * 1.0249^(x-20)
  return(predicted_ebu_rate)
}

COEFF_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = 89.82323 * 1.0598^(x-20)
  return(predicted_ebu_rate)
}

# eqn 25 & 26 --> Parameter Reservoir Diffusion
summary(res_diff_base)

COEFF_LOW_res_diff <- function(x){
  predicted_diff_rate = 13.90743 * 0.95941^(x-20)
  return(predicted_diff_rate)
}

COEFF_HIGH_res_diff <- function(x){
  predicted_diff_rate = 36.53811 * 1.09297^(x-20)
  return(predicted_diff_rate)
}

# eqn 27 & 28 --> Parameter Reservoir Ebullition
summary(res_ebu_base)

COEFF_LOW_res_ebu <- function(x){
  predicted_ebu_rate = 3.7361 * 1.2502^(x-20)
  return(predicted_ebu_rate)
}

COEFF_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = 113.3241 * 1.6052^(x-20)
  return(predicted_ebu_rate)
}


### MODEL VARIABILITY MODEL FITS ###
# eqn 29 & 30 --> Parameter Lake Diffusion
summary(lake_diff_base)

MODEL_LOW_lake_diff <- function(x){
  predicted_diff_rate = (23.631 * 1.081^(x-20)) - 66.83
  return(predicted_diff_rate)
}

MODEL_HIGH_lake_diff <- function(x){
  predicted_diff_rate = (23.631 * 1.081^(x-20)) + 66.83
  return(predicted_diff_rate)
}

# eqn 31 & 32 --> Parameter Lake Ebullition
summary(lake_ebu_base)

MODEL_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = (79.741 * 1.042^(x-20)) - 117.3
  return(predicted_ebu_rate)
}

MODEL_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = (79.741 * 1.042^(x-20)) + 117.3
  return(predicted_ebu_rate)
}


# eqn 33 & 34 --> Parameter Reservoir Diffusion
summary(res_diff_base)

MODEL_LOW_res_diff <- function(x){
  predicted_diff_rate = (25.223 * 1.026^(x-20)) - 96.11
  return(predicted_diff_rate)
}

MODEL_HIGH_res_diff <- function(x){
  predicted_diff_rate = (25.223 * 1.026^(x-20)) + 96.11
  return(predicted_diff_rate)
}

# eqn 35 & 36 --> Parameter Reservoir Ebullition
summary(res_ebu_base)

MODEL_LOW_res_ebu <- function(x){
  predicted_ebu_rate = (58.530 * 1.428^(x-20)) - 410.5
  return(predicted_ebu_rate)
}

MODEL_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = (58.530 * 1.428^(x-20)) + 410.5
  return(predicted_ebu_rate)
}

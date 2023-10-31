# Clear your environment and memory
rm(list=ls())
gc()

set.seed(1098)

d <- readr::read_csv("./output_data/GLEE_data_for_model_fit_with_error.csv") %>%
  mutate(temp_for_model_C = temp_for_model_K-273.15)

# ASSIGN THE DIFFERENT DATA FOR THE RESPECTIVE CALIBRATIONS

# lake diffusion data
lake_diff <- d %>% filter(waterbody_type == "lake") %>%
  select(ch4_diff,time_error_diff_lakes_low,time_error_diff_lakes_high,space_error_diff_lakes_low,space_error_diff_lakes_high,temp_for_model_C) %>%
  na.omit(.)

# lake ebullition data
lake_ebu <- d %>% filter(waterbody_type == "lake")%>%
  select(ch4_ebu,time_error_ebu_lakes_low,time_error_ebu_lakes_high,space_error_ebu_lakes_low,space_error_ebu_lakes_high,temp_for_model_C) %>%
  na.omit(.)

# reservoir diffusion
res_diff <- d %>% filter(waterbody_type == "reservoir") %>%
  select(ch4_diff,time_error_diff_res_low,time_error_diff_res_high,space_error_diff_res_low,space_error_diff_res_high,temp_for_model_C) %>%
  na.omit(.)

# reservoir ebullition
res_ebu <- d %>% filter(waterbody_type == "reservoir")%>%
  select(ch4_ebu,time_error_ebu_res_low,time_error_ebu_res_high,space_error_ebu_res_low,space_error_ebu_res_high,temp_for_model_C) %>%
  na.omit(.)

# FIT ALL 36 VARIABILITY SCENARIOS

### BASELINE MODEL FITS ###
# eqn 1 --> Baseline Lake Diffusion
lake_diff_base = nlsLM(ch4_diff ~ A * a^(temp_for_model_C-20),
                      start = list(A = 100, a = 1.1),
                      data = lake_diff,
                      control = nls.lm.control(maxiter=1000))

BASELINE_lake_diff <- function(x){
  predicted_diff_rate = 25.597 * 1.067^(x-20)
  return(predicted_diff_rate)
}

# eqn 2 --> Baseline Lake Ebullition
lake_ebu_base = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                       start = list(A = 100, a = 1.1),
                       data = lake_ebu,
                       control = nls.lm.control(maxiter=1000))

BASELINE_lake_ebu <- function(x){
  predicted_ebu_rate = 79.527 * 1.049^(x-20)
  return(predicted_ebu_rate)
}

# eqn 3 --> Baseline Reservoir Diffusion
res_diff_base = nlsLM(ch4_diff ~ A * a^(temp_for_model_C-20),
                       start = list(A = 100, a = 1.1),
                       data = res_diff,
                       control = nls.lm.control(maxiter=1000))

BASELINE_res_diff <- function(x){
  predicted_diff_rate = 24.836 * 1.026^(x-20)
  return(predicted_diff_rate)
}

# eqn 4 --> Baseline Reservoir Ebullition
res_ebu_base = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                      start = list(A = 100, a = 1.1),
                      data = res_ebu,
                      control = nls.lm.control(maxiter=1000))

BASELINE_res_ebu <- function(x){
  predicted_ebu_rate = 70.794 * 1.387^(x-20)
  return(predicted_ebu_rate)
}

### MEASUREMENT TEMPORAL VARIABILITY MODEL FITS ###

# eqn 5 --> Temporal Low Lake Diffusion
lake_diff_temp_low = nlsLM(time_error_diff_lakes_low ~ A * a^(temp_for_model_C-20),
                       start = list(A = 100, a = 1.1),
                       data = lake_diff,
                       control = nls.lm.control(maxiter=1000))

TIME_LOW_lake_diff <- function(x){
  predicted_diff_rate = 13.01 * 1.14^(x-20)
  return(predicted_diff_rate)
}

# eqn 6 --> Temporal High Lake Diffusion
lake_diff_temp_high = nlsLM(time_error_diff_lakes_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = lake_diff,
                           control = nls.lm.control(maxiter=1000))

TIME_HIGH_lake_diff <- function(x){
  predicted_diff_rate = 41.8783 * 0.9912^(x-20)
  return(predicted_diff_rate)
}

# eqn 7 --> Temporal low Lake Ebullition
lake_ebu_temp_low = nlsLM(time_error_ebu_lakes_low ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = lake_ebu,
                            control = nls.lm.control(maxiter=1000))

TIME_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = 53.987 * 1.065^(x-20)
  return(predicted_ebu_rate)
}

# eqn 8 --> Temporal High Lake Ebullition
lake_ebu_temp_high = nlsLM(time_error_ebu_lakes_high ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = lake_ebu,
                            control = nls.lm.control(maxiter=1000))

TIME_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = 142.061 * 1.038^(x-20)
  return(predicted_ebu_rate)
}

# eqn 9 --> Temporal Low Reservoir Diffusion
res_diff_temp_low = nlsLM(time_error_diff_res_low ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = res_diff,
                           control = nls.lm.control(maxiter=1000))

TIME_LOW_res_diff <- function(x){
  predicted_diff_rate = 18.812 * 1.011^(x-20)
  return(predicted_diff_rate)
}

# eqn 10 --> Temporal High Reservoir Diffusion
res_diff_temp_high = nlsLM(time_error_diff_res_high ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = res_diff,
                            control = nls.lm.control(maxiter=1000))

TIME_HIGH_res_diff <- function(x){
  predicted_diff_rate = 30.962 * 1.037^(x-20)
  return(predicted_diff_rate)
}

# eqn 11 --> Temporal Low Reservoir Ebullition
res_ebu_temp_low = nlsLM(time_error_ebu_res_low ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = res_ebu,
                          control = nls.lm.control(maxiter=1000))

TIME_LOW_res_ebu <- function(x){
  predicted_ebu_rate = 25.569 * 1.372^(x-20)
  return(predicted_ebu_rate)
}

# eqn 12 --> Temporal High Reservoir Ebullition
res_ebu_temp_high = nlsLM(time_error_ebu_res_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = res_ebu,
                           control = nls.lm.control(maxiter=1000))

TIME_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = 123.471 * 1.457^(x-20)
  return(predicted_ebu_rate)
}


### MEASUREMENT SPATIAL VARIABILITY MODEL FITS ###

# eqn 13 --> Spatial Low Lake Diffusion
lake_diff_space_low = nlsLM(space_error_diff_lakes_low ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = lake_diff,
                           control = nls.lm.control(maxiter=1000))

SPACE_LOW_lake_diff <- function(x){
  predicted_diff_rate = 11.754 * 1.147^(x-20)
  return(predicted_diff_rate)
}

# eqn 14 --> Spatial High Lake Diffusion
lake_diff_space_high = nlsLM(space_error_diff_lakes_high ~ A * a^(temp_for_model_C-20),
                            start = list(A = 100, a = 1.1),
                            data = lake_diff,
                            control = nls.lm.control(maxiter=1000))

SPACE_HIGH_lake_diff <- function(x){
  predicted_diff_rate = 45.5616 * 0.9996^(x-20)
  return(predicted_diff_rate)
}

# eqn 15 --> Spatial low Lake Ebullition
lake_ebu_space_low = nlsLM(space_error_ebu_lakes_low ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = lake_ebu,
                          control = nls.lm.control(maxiter=1000))

SPACE_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = 51.747 * 1.071^(x-20)
  return(predicted_ebu_rate)
}

# eqn 16 --> Spatial High Lake Ebullition
lake_ebu_space_high = nlsLM(space_error_ebu_lakes_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = lake_ebu,
                           control = nls.lm.control(maxiter=1000))

SPACE_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = 143.068 * 1.037^(x-20)
  return(predicted_ebu_rate)
}

# eqn 17 --> Spatial Low Reservoir Diffusion
res_diff_space_low = nlsLM(space_error_diff_res_low ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = res_diff,
                          control = nls.lm.control(maxiter=1000))

SPACE_LOW_res_diff <- function(x){
  predicted_diff_rate = 20.45 * 1.02^(x-20)
  return(predicted_diff_rate)
}

# eqn 18 --> Spatial High Reservoir Diffusion
res_diff_space_high = nlsLM(space_error_diff_res_high ~ A * a^(temp_for_model_C-20),
                           start = list(A = 100, a = 1.1),
                           data = res_diff,
                           control = nls.lm.control(maxiter=1000))

SPACE_HIGH_res_diff <- function(x){
  predicted_diff_rate = 30.571 * 1.032^(x-20)
  return(predicted_diff_rate)
}


# eqn 19 --> Spatial Low Reservoir Ebullition
res_ebu_space_low = nlsLM(space_error_ebu_res_low ~ A * a^(temp_for_model_C-20),
                         start = list(A = 100, a = 1.1),
                         data = res_ebu,
                         control = nls.lm.control(maxiter=1000))

SPACE_LOW_res_ebu <- function(x){
  predicted_ebu_rate = 5.878 * 1.694^(x-20)
  return(predicted_ebu_rate)
}

# eqn 20 --> Spatial High Reservoir Ebullition
res_ebu_space_high = nlsLM(space_error_ebu_res_high ~ A * a^(temp_for_model_C-20),
                          start = list(A = 100, a = 1.1),
                          data = res_ebu,
                          control = nls.lm.control(maxiter=1000))

SPACE_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = 164.471 * 1.395^(x-20)
  return(predicted_ebu_rate)
}


### COEFFICIENT VARIABILITY MODEL FITS ###
# eqn 21 & 22 --> COEFFICIENT Lake Diffusion
summary(lake_diff_base)

COEFF_LOW_lake_diff <- function(x){
  predicted_diff_rate = 21.7846 * 1.04804^(x-20)
  return(predicted_diff_rate)
}

COEFF_HIGH_lake_diff <- function(x){
  predicted_diff_rate = 29.40922 * 1.08642^(x-20)
  return(predicted_diff_rate)
}

# eqn 23 & 24 --> Parameter Lake Ebullition
summary(lake_ebu_base)

COEFF_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = 69.40237 * 1.03113^(x-20)
  return(predicted_ebu_rate)
}

COEFF_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = 89.65203 * 1.06619^(x-20)
  return(predicted_ebu_rate)
}

# eqn 25 & 26 --> Parameter Reservoir Diffusion
summary(res_diff_base)

COEFF_LOW_res_diff <- function(x){
  predicted_diff_rate = 13.56328 * 0.95728^(x-20)
  return(predicted_diff_rate)
}

COEFF_HIGH_res_diff <- function(x){
  predicted_diff_rate = 36.10828 * 1.09558^(x-20)
  return(predicted_diff_rate)
}

# eqn 27 & 28 --> Parameter Reservoir Ebullition
summary(res_ebu_base)

COEFF_LOW_res_ebu <- function(x){
  predicted_ebu_rate = 0.1 * 1.194^(x-20)
  return(predicted_ebu_rate)
}

COEFF_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = 144.9004 * 1.579^(x-20)
  return(predicted_ebu_rate)
}


### MODEL VARIABILITY MODEL FITS ###
# eqn 29 & 30 --> Parameter Lake Diffusion
summary(lake_diff_base)

MODEL_LOW_lake_diff <- function(x){
  predicted_diff_rate = (23.631 * 1.081^(x-20)) - 69.09
  return(predicted_diff_rate)
}

MODEL_HIGH_lake_diff <- function(x){
  predicted_diff_rate = (23.631 * 1.081^(x-20)) + 69.09
  return(predicted_diff_rate)
}

# eqn 31 & 32 --> Parameter Lake Ebullition
summary(lake_ebu_base)

MODEL_LOW_lake_ebu <- function(x){
  predicted_ebu_rate = (79.741 * 1.042^(x-20)) - 116.4
  return(predicted_ebu_rate)
}

MODEL_HIGH_lake_ebu <- function(x){
  predicted_ebu_rate = (79.741 * 1.042^(x-20)) + 116.4
  return(predicted_ebu_rate)
}


# eqn 33 & 34 --> Parameter Reservoir Diffusion
summary(res_diff_base)

MODEL_LOW_res_diff <- function(x){
  predicted_diff_rate = (25.223 * 1.026^(x-20)) - 95.64
  return(predicted_diff_rate)
}

MODEL_HIGH_res_diff <- function(x){
  predicted_diff_rate = (25.223 * 1.026^(x-20)) + 95.64
  return(predicted_diff_rate)
}

# eqn 35 & 36 --> Parameter Reservoir Ebullition
summary(res_ebu_base)

MODEL_LOW_res_ebu <- function(x){
  predicted_ebu_rate = (58.530 * 1.428^(x-20)) - 425.2
  return(predicted_ebu_rate)
}

MODEL_HIGH_res_ebu <- function(x){
  predicted_ebu_rate = (58.530 * 1.428^(x-20)) + 425.2
  return(predicted_ebu_rate)
}

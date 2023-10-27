# Clear your environment and memory
rm(list=ls())
gc()

# read in data set that has been linked to GLCP Hydrobasin climate data
base <- read_csv("./source_data/GLEE_data_with_GLCP_HWSD_link.csv")


filtered_lakes <- base %>%
  ## --> quantify the number of total samples from each site by multiplying the
  ## --> number of months sampled by the number of sites collecting CH4 emissions
  mutate(tot_sampling_events = num_months_sampled * num_sites_sampled) %>%
  group_by(geometry, waterbody_type) %>%
  ## --> calculate a waterbody_id column for each individual lakes
  ## --> this will convey observation uncertainty by individual lakes
  mutate(waterbody_id = cur_group_id()) %>%
  ## --> remove any lakes that have only one measurement
  #filter(any(length(waterbody_id)>1)) %>%
  ungroup() %>%
  ## --> assign the temperature used for the model to find either the observed air temp 
  ## --> or the air temperature as determined by the GCLP hydrobasin
  mutate(temp_for_model_K = ifelse(is.na(mean_temp_k), effective_obs_wtemp_k, mean_temp_k)) %>%
  group_by(waterbody_id) %>%
  mutate(temp_for_model_K = ifelse(n_distinct(effective_obs_wtemp_k) == 0, mean_temp_k, effective_obs_wtemp_k))


# TEMPORAL MEASUREMENT UNCERTAINTY
# ebullition
time_base_ebu <- filtered_lakes %>% ungroup(.) %>%
  select(num_months_sampled, ch4_ebu, ch4_diff) %>% 
  melt(., id.vars = "num_months_sampled") %>%
  na.omit(.) %>%
  filter(variable == "ch4_ebu") %>%
  filter(value > 0)

mean_time_ebu <- 10^mean(log10(time_base_ebu$value))

sd_time_ebu <- time_base_ebu %>% group_by(num_months_sampled) %>%
  summarise_at(vars(value),
               list(sd = sd)) %>%
  ungroup(.) %>%
  mutate(mean_time_ebu = mean_time_ebu)

sd_time_ebu <- time_base_ebu %>% group_by(num_months_sampled) %>%
  summarise(sd = 10^sd(log10(value))) %>%
  ungroup(.) %>%
  mutate(mean_time_ebu = mean_time_ebu)

# diffusion
time_base_diff <- filtered_lakes %>% ungroup(.) %>%
  select(num_months_sampled, ch4_ebu, ch4_diff) %>% 
  melt(., id.vars = "num_months_sampled") %>%
  na.omit(.) %>%
  filter(variable == "ch4_diff")%>%
  filter(value > 0)



# assign a vector that is associated with the # of total duration
error_diff_months <- as.data.frame(seq(1,48, by = 1)) %>%
  rename(num_months_sampled = `seq(1, 48, by = 1)`)

# define the lowest values from that vector of values
lowest_diff_months <- c(-2.72, -1.49, -0.24, -0.10)

# create the lowest possible values attainable from these measurements
error_diff_min <- filtered_lakes %>% ungroup(.) %>% select(num_months_sampled, ch4_diff) %>% 
  na.omit(.) %>%
  mutate(num_months_sampled = round(num_months_sampled)) %>%
  ungroup(.) %>%
  group_by(num_months_sampled) %>%
  filter(ch4_diff > 0) %>%
  summarize(value = min(ch4_diff)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% lowest_diff_months) %>%
  filter(num_months_sampled>=9)


low_bound_diff_time <- left_join(error_diff_months, error_diff_min, by = "num_months_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value))%>%
  mutate(model = "diff_lower_time_error")

highest_diff_months <- c(3.72, 1.48)

error_diff_max <- filtered_lakes %>% ungroup(.) %>% select(num_months_sampled, ch4_diff) %>% 
  filter(num_months_sampled < 100) %>%
  na.omit(.) %>%
  mutate(num_months_sampled = round(num_months_sampled)) %>%
  ungroup(.) %>%
  group_by(num_months_sampled) %>%
  summarize(value = max(ch4_diff)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% highest_diff_months)


high_bound_diff_time <- left_join(error_diff_months, error_diff_max, by = "num_months_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value))%>%
  mutate(model = "diff_higher_time_error")

error_diff_time <- rbind(low_bound_diff_time, high_bound_diff_time)%>%
  mutate(flux = "diffusion")%>%
  mutate(value = 10^value)


error_ebu_months <- as.data.frame(seq(1,48, by = 1)) %>%
  rename(num_months_sampled = `seq(1, 48, by = 1)`)

lowest_ebu_months <- c(-1.79,-1.72, -1.40, -0.59)

error_ebu_min <- filtered_lakes %>% ungroup(.) %>% select(num_months_sampled, ch4_ebu) %>% 
  na.omit(.) %>%
  mutate(num_months_sampled = round(num_months_sampled)) %>%
  ungroup(.) %>%
  group_by(num_months_sampled) %>%
  filter(ch4_ebu > 0) %>%
  summarize(value = min(ch4_ebu)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% lowest_ebu_months)

low_bound_ebu_time <- left_join(error_ebu_months, error_ebu_min, by = "num_months_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value))%>%
  mutate(model = "ebu_lower_time_error")

highest_ebu_months <- c(3.38, 2.66, 2.06)

error_ebu_max <- filtered_lakes %>% ungroup(.) %>% select(num_months_sampled, ch4_ebu) %>% 
  filter(num_months_sampled < 100) %>%
  na.omit(.) %>%
  mutate(num_months_sampled = round(num_months_sampled)) %>%
  ungroup(.) %>%
  group_by(num_months_sampled) %>%
  summarize(value = max(ch4_ebu)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% highest_ebu_months)

high_bound_ebu_time <- left_join(error_ebu_months, error_ebu_max, by = "num_months_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value)) %>%
  mutate(model = "ebu_upper_time_error")

error_ebu_time <- rbind(low_bound_ebu_time, high_bound_ebu_time) %>%
  mutate(flux = "ebullition")%>%
  mutate(value = 10^value)

time_models <- rbind(error_ebu_time, error_diff_time) 

time_error_ebu <- ggplot(sd_time_ebu, aes(num_months_sampled, mean_time_ebu), color = "black")+
  geom_jitter(data = time_base_ebu, aes(x = num_months_sampled, y = value, color = variable), 
              fill = "grey", size = 2, pch = 21, inherit.aes = F, width = 0.1)+
  geom_line(lwd = 2)+
  geom_errorbar(aes(ymin = mean_time_ebu-2*sd, ymax = mean_time_ebu+2*sd), width = 0.25)+
  scale_color_manual(values = c("blue", "blue4"))+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("Range of Potential Flux Variability")+
  labs(title = "Ebullition")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

time_error_diff <- ggplot(error_diff_time, aes(num_months_sampled, value, color = model))+
  geom_jitter(data = time_base_diff, aes(x = num_months_sampled, y = value, color = variable), 
              fill = "grey", size = 2, pch = 21, inherit.aes = F)+
  geom_line(lwd = 2)+
  scale_color_manual(values = c("blue", "blue4","blue4"))+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("Duration of Sampling (Months)")+
  ylab("Range of Potential Flux Variability")+
  labs(title = "Diffusion")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))


space_base_ebu <- filtered_lakes %>% ungroup(.) %>%
  select(num_sites_sampled, ch4_ebu, ch4_diff) %>% 
  filter(num_sites_sampled < 100) %>%
  melt(., id.vars = "num_sites_sampled") %>%
  na.omit(.)%>%
  filter(variable == "ch4_ebu")%>%
  filter(value > 0)

space_base_diff <- filtered_lakes %>% ungroup(.) %>%
  select(num_sites_sampled, ch4_ebu, ch4_diff) %>% 
  filter(num_sites_sampled < 100) %>%
  melt(., id.vars = "num_sites_sampled") %>%
  na.omit(.) %>%
  filter(variable == "ch4_diff")%>%
  filter(value > 0)

error_diff_sites <- as.data.frame(seq(1,80, by = 1)) %>%
  rename(num_sites_sampled = `seq(1, 80, by = 1)`)

lowest_diff_sites <- c(-2.72, -2.00, -1.19, -0.79)

error_diff_min <- filtered_lakes %>% ungroup(.) %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled < 100) %>%
  na.omit(.) %>%
  mutate(num_sites_sampled = round(num_sites_sampled)) %>%
  ungroup(.) %>%
  group_by(num_sites_sampled) %>%
  summarize(value = min(ch4_diff)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% lowest_diff_sites) %>%
  filter(num_sites_sampled != 10)

low_bound_diff_space <- left_join(error_diff_sites, error_diff_min, by = "num_sites_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value))%>%
  mutate(model = "diff_lower_space_error")

highest_diff_sites <- c(3.72, 2.77, 1.02, 0.09)

error_diff_max <- filtered_lakes %>% ungroup(.) %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled < 100) %>%
  na.omit(.) %>%
  mutate(num_sites_sampled = round(num_sites_sampled)) %>%
  ungroup(.) %>%
  group_by(num_sites_sampled) %>%
  summarize(value = max(ch4_diff)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% highest_diff_sites)


high_bound_diff_space <- left_join(error_diff_sites, error_diff_max, by = "num_sites_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value))%>%
  mutate(model = "diff_higher_space_error")

error_diff_space <- rbind(low_bound_diff_space, high_bound_diff_space)%>%
  mutate(flux = "diffusion")%>%
  mutate(value = 10^value)


error_ebu_sites <- as.data.frame(seq(1,80, by = 1)) %>%
  rename(num_sites_sampled = `seq(1, 80, by = 1)`)

lowest_ebu_sites <- c(-1.79, -1.40, -0.59, 0.23)

error_ebu_min <- filtered_lakes %>% ungroup(.) %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled < 100) %>%
  na.omit(.) %>%
  mutate(num_sites_sampled = round(num_sites_sampled)) %>%
  ungroup(.) %>%
  group_by(num_sites_sampled) %>%
  filter(ch4_ebu > 0) %>%
  summarize(value = min(ch4_ebu)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% lowest_ebu_sites)

low_bound_ebu_space <- left_join(error_ebu_sites, error_ebu_min, by = "num_sites_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value))%>%
  mutate(model = "ebu_lower_space_error")

highest_ebu_sites <- c(3.38, 2.87, 2.28)

error_ebu_max <- filtered_lakes %>% ungroup(.) %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled < 100) %>%
  na.omit(.) %>%
  mutate(num_sites_sampled = round(num_sites_sampled)) %>%
  ungroup(.) %>%
  group_by(num_sites_sampled) %>%
  summarize(value = max(ch4_ebu)) %>%
  mutate(value = log10(value)) %>%
  mutate(value = round(value, digits =2)) %>%
  subset(value %in% highest_ebu_sites)


high_bound_ebu_space <- left_join(error_ebu_sites, error_ebu_max, by = "num_sites_sampled") %>%
  ungroup(.) %>%
  mutate(value = na_interpolation(value)) %>%
  mutate(model = "ebu_upper_space_error")

error_ebu_space <- rbind(low_bound_ebu_space, high_bound_ebu_space) %>%
  mutate(flux = "ebullition")%>%
  mutate(value = 10^value)

space_models <- rbind(error_ebu_space, error_diff_space) 

space_error_ebu <- ggplot(error_ebu_space, aes(num_sites_sampled, value, color = model))+
  geom_jitter(data = space_base_ebu, aes(x = num_sites_sampled, y = value, color = variable), 
              fill = "grey", size = 2, pch = 21, inherit.aes = F)+
  geom_line(lwd = 2)+
  scale_color_manual(values = c("red", "red4","red4"))+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Ebullition")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

space_error_diff <- ggplot(error_diff_space, aes(num_sites_sampled, value, color = model))+
  geom_jitter(data = space_base_diff, aes(x = num_sites_sampled, y = value, color = variable), 
              fill = "grey", size = 2, pch = 21, inherit.aes = F)+
  geom_line(lwd = 2)+
  scale_color_manual(values = c("red", "red4","red4"))+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("Extent of Sampling (# Sites)")+
  ylab("")+
  labs(title = "Diffusion")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))


ylab <- time_error_ebu$labels$y
time_error_ebu$labels$y <- time_error_diff$labels$y <- " "



(time_error_ebu + space_error_ebu) /
(time_error_diff + space_error_diff)
grid::grid.draw(grid::textGrob(ylab, x = 0.02, rot = 90))

ggsave("./figures/spatiotemporal_variability.jpeg", device = "jpeg", dpi = 1000, width = 8, height = 5, units = "in")

# Calculate the new potential high and low value of flux based on number of sites and months sampled

error_ebu_months_low <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_months_sampled, ch4_ebu, temp_for_model_K) %>% 
  left_join(., low_bound_ebu_time, by = "num_months_sampled") %>%
  mutate(ch4_ebu_time_low = 10^value) %>%
  select(-value, -model)

error_ebu_months_high <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_months_sampled, ch4_ebu, temp_for_model_K) %>% 
  left_join(., high_bound_ebu_time, by = "num_months_sampled") %>%
  mutate(ch4_ebu_time_high = 10^value) %>%
  select(-value, -model)

error_ebu_sites_low <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_sites_sampled, ch4_ebu, temp_for_model_K) %>% 
  left_join(., low_bound_ebu_space, by = "num_sites_sampled") %>%
  mutate(ch4_ebu_space_low = 10^value) %>%
  select(-value, -model)

error_ebu_sites_high <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_sites_sampled, ch4_ebu, temp_for_model_K) %>% 
  left_join(., high_bound_ebu_space, by = "num_sites_sampled") %>%
  mutate(ch4_ebu_space_high = 10^value) %>%
  select(-value, -model)


error_diff_months_low <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_months_sampled, ch4_diff, temp_for_model_K) %>% 
  left_join(., low_bound_diff_time, by = "num_months_sampled") %>%
  mutate(ch4_diff_time_low = 10^value) %>%
  select(-value, -model)

error_diff_months_high <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_months_sampled, ch4_diff, temp_for_model_K) %>% 
  left_join(., high_bound_diff_time, by = "num_months_sampled") %>%
  mutate(ch4_diff_time_high = 10^value) %>%
  select(-value, -model)

error_diff_sites_low <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_sites_sampled, ch4_diff, temp_for_model_K) %>% 
  left_join(., low_bound_diff_space, by = "num_sites_sampled") %>%
  mutate(ch4_diff_space_low = 10^value) %>%
  select(-value, -model)

error_diff_sites_high <- filtered_lakes %>% ungroup(.) %>%
  select(waterbody_id, waterbody_type, num_sites_sampled, ch4_diff, temp_for_model_K) %>% 
  left_join(., high_bound_diff_space, by = "num_sites_sampled") %>%
  mutate(ch4_diff_space_high = 10^value) %>%
  select(-value, -model)


cbind(filtered_lakes, 
      error_ebu_months_low[6],error_ebu_months_high[6],
      error_ebu_sites_low[6],error_ebu_sites_high[6],
      error_diff_months_low[6],error_diff_months_high[6],
      error_diff_sites_low[6],error_diff_sites_high[6]) %>%
  readr::write_csv(., "./output_data/GLEE_data_for_model_fitting.csv")



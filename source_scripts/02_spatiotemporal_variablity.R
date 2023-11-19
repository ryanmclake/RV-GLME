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
# ebullition lakes
time_base_ebu_lake <- filtered_lakes %>% ungroup(.) %>%
  select(num_months_sampled, ch4_ebu, ch4_diff, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "lake") %>%
  melt(., id.vars = c("num_months_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_ebu") %>%
  filter(value > 0 & value != 497.24000) %>%
  ungroup(.)

sd_time_ebu_lake <- time_base_ebu_lake %>% group_by(num_months_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean)) %>%
  na.omit(.)

time_error_ebu_lakes <- ggplot(data = time_base_ebu_lake, aes(x = num_months_sampled, y = value))+
  geom_point(size = 1, pch = 21, fill = "blue")+
  geom_point(data = sd_time_ebu_lake, aes(x = num_months_sampled, y = max), size = 4,pch=21, fill = "darkblue")+
  geom_point(data = sd_time_ebu_lake, aes(x = num_months_sampled, y = min), size = 4,pch=21, fill = "cyan")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("Number of Months Sampled")+
  ylab("Range of Potential Flux Variability")+
  labs(title = "Ebullition Lakes")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))


# diffusion lakes
time_base_diff_lake <- filtered_lakes %>% ungroup(.) %>%
  select(num_months_sampled, ch4_diff, ch4_diff, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "lake") %>%
  melt(., id.vars = c("num_months_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_diff") %>%
  filter(value > 0) %>%
  ungroup(.)

sd_time_diff_lake <- time_base_diff_lake %>% group_by(num_months_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean)) %>%
  na.omit(.)

time_error_diff_lakes <- ggplot(data = time_base_diff_lake, aes(x = num_months_sampled, y = value))+
  geom_point(size = 1, pch = 21, fill = "blue")+
  geom_point(data = sd_time_diff_lake, aes(x = num_months_sampled, y = max), size = 4,pch=21, fill = "darkblue")+
  geom_point(data = sd_time_diff_lake, aes(x = num_months_sampled, y = min), size = 4,pch=21, fill = "cyan")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Diffusion Lakes")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# ebullition reservoirs
time_base_ebu_res <- filtered_lakes %>% ungroup(.) %>%
  select(num_months_sampled, ch4_ebu, ch4_ebu, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "reservoir") %>%
  melt(., id.vars = c("num_months_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_ebu" & num_months_sampled != 14 &num_months_sampled != 18) %>%
  filter(value > 0) %>%
  ungroup(.)

sd_time_ebu_res <- time_base_ebu_res %>% group_by(num_months_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean)) %>%
  na.omit(.)

time_error_ebu_res <- ggplot(data = time_base_ebu_res, aes(x = num_months_sampled, y = value))+
  geom_point(size = 1, pch = 24, fill = "orange")+
  geom_point(data = sd_time_ebu_res, aes(x = num_months_sampled, y = max), size = 4,pch=21, fill = "darkblue")+
  geom_point(data = sd_time_ebu_res, aes(x = num_months_sampled, y = min), size = 4,pch=21, fill = "cyan")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Ebullition Reservoirs")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# diffusion reservoirs
time_base_diff_res <- filtered_lakes %>% ungroup(.) %>%
  select(num_months_sampled, ch4_diff, ch4_diff, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "reservoir") %>%
  melt(., id.vars = c("num_months_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_diff" & num_months_sampled != 14 &num_months_sampled != 18) %>%
  filter(value > 0) %>%
  ungroup(.)

sd_time_diff_res <- time_base_diff_res %>% group_by(num_months_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean)) %>%
  na.omit(.)

time_error_diff_res <- ggplot(data = time_base_diff_res, aes(x = num_months_sampled, y = value))+
  geom_point(size = 1, pch = 24, fill = "orange")+
  geom_point(data = sd_time_diff_res, aes(x = num_months_sampled, y = max), size = 4,pch=21, fill = "darkblue")+
  geom_point(data = sd_time_diff_res, aes(x = num_months_sampled, y = min), size = 4,pch=21, fill = "cyan")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Diffusion Reservoirs")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))




# SPATIAL MEASUREMENT UNCERTAINTY
# ebullition lakes
space_base_ebu_lake <- filtered_lakes %>% ungroup(.) %>%
  select(num_sites_sampled, ch4_ebu, ch4_diff, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "lake") %>%
  melt(., id.vars = c("num_sites_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_ebu" & num_sites_sampled != 13 &
           num_sites_sampled != 27 & num_sites_sampled != 30 & num_sites_sampled != 41) %>%
  ungroup(.)

sd_space_ebu_lake <- space_base_ebu_lake %>% group_by(num_sites_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean)) %>%
  na.omit(.)

space_error_ebu_lakes <- ggplot(data = space_base_ebu_lake, aes(x = num_sites_sampled, y = value))+
  geom_point(size = 1, pch = 21, fill = "blue")+
  geom_point(data = sd_space_ebu_lake, aes(x = num_sites_sampled, y = max), size = 4,pch=21, fill = "red4")+
  geom_point(data = sd_space_ebu_lake, aes(x = num_sites_sampled, y = min), size = 4,pch=21, fill = "pink")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("Number of Sites Sampled")+
  ylab("")+
  labs(title = "Ebullition Lakes")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))


# diffusion lakes
space_base_diff_lake <- filtered_lakes %>% ungroup(.) %>%
  select(num_sites_sampled, ch4_diff, ch4_diff, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "lake") %>%
  melt(., id.vars = c("num_sites_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_diff" & num_sites_sampled<200) %>%
  filter(value > 0) %>%
  ungroup(.)

sd_space_diff_lake <- space_base_diff_lake %>% group_by(num_sites_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean))

singles <- sd_space_diff_lake %>% filter(is.na(mean))

space_error_diff_lakes <- ggplot(data = space_base_diff_lake, aes(x = num_sites_sampled, y = value))+
  geom_point(size = 1, pch = 21, fill = "blue")+
  geom_point(data = sd_space_diff_lake, aes(x = num_sites_sampled, y = max), size = 4,pch=21, fill = "red4")+
  geom_point(data = sd_space_diff_lake, aes(x = num_sites_sampled, y = min), size = 4,pch=21, fill = "pink")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Diffusion Lakes")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# ebullition reservoirs
space_base_ebu_res <- filtered_lakes %>% ungroup(.) %>%
  select(num_sites_sampled, ch4_ebu, ch4_ebu, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "reservoir") %>%
  melt(., id.vars = c("num_sites_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_ebu" & num_sites_sampled != 14 &num_sites_sampled != 18) %>%
  filter(value > 0) %>%
  ungroup(.)

sd_space_ebu_res <- space_base_ebu_res %>% group_by(num_sites_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean))

singles <- sd_space_ebu_res %>% filter(is.na(mean))

space_error_ebu_res <- ggplot(data = space_base_ebu_res, aes(x = num_sites_sampled, y = value))+
  geom_point(size = 1, pch = 24, fill = "orange")+
  geom_point(data = sd_space_ebu_res, aes(x = num_sites_sampled, y = max), size = 4,pch=21, fill = "red4")+
  geom_point(data = sd_space_ebu_res, aes(x = num_sites_sampled, y = min), size = 4,pch=21, fill = "pink")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Ebullition Reservoirs")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# diffusion reservoirs
space_base_diff_res <- filtered_lakes %>% ungroup(.) %>%
  select(num_sites_sampled, ch4_diff, ch4_diff, waterbody_type) %>% 
  filter(waterbody_type != "pond" & waterbody_type == "reservoir") %>%
  melt(., id.vars = c("num_sites_sampled","waterbody_type")) %>%
  na.omit(.) %>%
  filter(variable == "ch4_diff" & num_sites_sampled != 14 &num_sites_sampled != 18) %>%
  filter(value > 0) %>%
  ungroup(.)

sd_space_diff_res <- space_base_diff_res %>% group_by(num_sites_sampled) %>%
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75)) %>%
  mutate(mean = ifelse(max == mean, NA, mean)) %>%
  na.omit(.)

space_error_diff_res <- ggplot(data = space_base_diff_res, aes(x = num_sites_sampled, y = value))+
  geom_point(size = 1, pch = 24, fill = "orange")+
  geom_point(data = sd_space_diff_res, aes(x = num_sites_sampled, y = max), size = 4,pch=21, fill = "red4")+
  geom_point(data = sd_space_diff_res, aes(x = num_sites_sampled, y = min), size = 4,pch=21, fill = "pink")+
  scale_y_log10(breaks = 10^(-4:4), labels = trans_format("log10", math_format(10^.x)))+
  xlab("")+
  ylab("")+
  labs(title = "Diffusion Reservoirs")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ylab <- time_error_ebu_lakes$labels$y
time_error_ebu_lakes$labels$y <- time_error_ebu_lakes$labels$y <- " "

xlab1 <- time_error_ebu_lakes$labels$x
time_error_ebu_lakes$labels$x <- time_error_ebu_lakes$labels$x <- " "

xlab2 <- space_error_ebu_lakes$labels$x
space_error_ebu_lakes$labels$x <- space_error_ebu_lakes$labels$x <- " "

(time_error_diff_lakes | time_error_diff_res | time_error_ebu_lakes | time_error_ebu_res) /
(space_error_diff_lakes | space_error_diff_res | space_error_ebu_lakes | space_error_ebu_res)
grid::grid.draw(grid::textGrob(ylab, x = 0.02, rot = 90))
grid::grid.draw(grid::textGrob(xlab1, y = 0.51, rot = 0))
grid::grid.draw(grid::textGrob(xlab2, y = 0.02, rot = 0))

ggsave("./figures/spatiotemporal_variability.jpeg", device = "jpeg", dpi = 1000, width = 16, height = 6, units = "in")


time_filtered_lakes_error <- filtered_lakes %>%
  group_by(num_months_sampled, waterbody_id) %>%
  mutate(time_error_diff_lakes_low = ifelse(waterbody_type == "lake", min(ch4_diff), NA),
         time_error_diff_lakes_high = ifelse(waterbody_type == "lake", max(ch4_diff), NA),
         time_error_diff_res_low = ifelse(waterbody_type == "reservoir", min(ch4_diff), NA),
         time_error_diff_res_high = ifelse(waterbody_type == "reservoir", max(ch4_diff), NA),
         time_error_ebu_lakes_low = ifelse(waterbody_type == "lake", min(ch4_ebu), NA),
         time_error_ebu_lakes_high = ifelse(waterbody_type == "lake", max(ch4_ebu), NA),
         time_error_ebu_res_low = ifelse(waterbody_type == "reservoir", min(ch4_ebu), NA),
         time_error_ebu_res_high = ifelse(waterbody_type == "reservoir", max(ch4_ebu), NA))
         
filtered_lakes_error_all <- filtered_lakes %>%
  group_by(num_sites_sampled, waterbody_id) %>%
  mutate(space_error_diff_lakes_low = ifelse(waterbody_type == "lake", min(ch4_diff), NA),
         space_error_diff_lakes_high = ifelse(waterbody_type == "lake", max(ch4_diff), NA),
         space_error_diff_res_low = ifelse(waterbody_type == "reservoir", min(ch4_diff), NA),
         space_error_diff_res_high = ifelse(waterbody_type == "reservoir", max(ch4_diff), NA),
         space_error_ebu_lakes_low = ifelse(waterbody_type == "lake", min(ch4_ebu), NA),
         space_error_ebu_lakes_high = ifelse(waterbody_type == "lake", max(ch4_ebu), NA),
         space_error_ebu_res_low = ifelse(waterbody_type == "reservoir", min(ch4_ebu), NA),
         space_error_ebu_res_high = ifelse(waterbody_type == "reservoir", max(ch4_ebu), NA)) %>%
  left_join(., time_filtered_lakes_error) %>%
  readr::write_csv("./output_data/GLEE_data_for_model_fit_with_error.csv")

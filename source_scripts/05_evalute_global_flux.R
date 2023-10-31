
# Clear your environment and memory
rm(list=ls())
gc()


diffusion_prediction <- vroom::vroom("./output_data/japan_diffusion_prediction.csv")
summary(diffusion_prediction)

diffusion_lakes <- diffusion_prediction %>% filter(lake_type == 1) %>%
  ggplot(., aes(date, BASELINE_diffusion, group = hylak_id))+
  geom_line()

diffusion_reservoirs <- diffusion_prediction %>% filter(lake_type == 2 | lake_type == 3) %>%
  ggplot(., aes(date, BASELINE_diffusion, group = hylak_id))+
  geom_line()

ebullition_prediction <- vroom::vroom("./output_data/japan_ebullition_prediction.csv")
summary(ebullition_prediction)

ebullition_lakes <- ebullition_prediction %>% filter(lake_type == 1) %>%
  ggplot(., aes(date, BASELINE_ebullition, group = hylak_id))+
  geom_line()

ebullition_reservoirs <- ebullition_prediction %>% filter(lake_type == 2 | lake_type == 3)


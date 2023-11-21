
# Clear your environment and memory
rm(list=ls())
gc()

### PLOT TIME SERIES OF THE FLUXES ###

diffusion_prediction <- vroom::vroom("./output_data/belgium_diffusion_prediction.csv")
summary(diffusion_prediction)

diffusion_lakes <- diffusion_prediction %>% filter(lake_type == 1)

ggplot(diffusion_lakes, aes(date, BASELINE_diffusion, group = hylak_id))+
  geom_line()

diffusion_reservoirs <- diffusion_prediction %>% filter(lake_type == 2 | lake_type == 3) 

ggplot(diffusion_reservoirs, aes(date, BASELINE_diffusion, group = hylak_id))+
  geom_line()

ebullition_prediction <- vroom::vroom("./output_data/belgium_ebullition_prediction.csv")
summary(ebullition_prediction)

ebullition_lakes <- ebullition_prediction %>% filter(lake_type == 1)

ggplot(ebullition_lakes, aes(date, BASELINE_ebullition, group = hylak_id))+
  geom_line()

ebullition_reservoirs <- ebullition_prediction %>% filter(lake_type == 2 | lake_type == 3)

ggplot(ebullition_reservoirs, aes(date, BASELINE_ebullition, group = hylak_id))+
  geom_line()

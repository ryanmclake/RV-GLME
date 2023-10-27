
# Clear your environment and memory
rm(list=ls())
gc()


diffusion_prediction <- vroom::vroom("./output_data/japan_diffusion_prediction.csv")
summary(diffusion_prediction)

ebullition_prediction <- vroom::vroom("./output_data/japan_ebullition_prediction.csv")
summary(ebullition_prediction)


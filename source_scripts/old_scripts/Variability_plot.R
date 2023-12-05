
library(tidyverse)

dat <- read_csv("./output_data/global_predictions.csv") %>%
  rename(`Variability Source` = group)


summary(dat)

cols <- c("a: Past Estimates" = "black",
          "b: Baseline Estimate" = "green", 
          "c: Low Temporal Fluxes" = "lightblue2", 
          "d: High Temporal Fluxes" = "blue2",
          "e: Low Spatial Fluxes" = "pink2", 
          "f: High Spatial Fluxes" = "red2",
          "g: Parameter -1SD" = "darkgoldenrod1",
          "h: Parameter +1SD" = "darkorange3",
          "i: Model -1RSD" = "grey70", 
          "j: Model +1RSD" = "grey40")


p <- ggplot(dat, aes(x = area_weighted_flux, y = author, group =`Variability Source`,  fill = `Variability Source`))+
  scale_y_discrete(limits=rev)+
  theme_classic() + 
  scale_colour_manual(values = cols)+
  scale_fill_manual(values = cols)+
  geom_segment(aes(x = 63 , y = 6, xend = 63, yend = 5), lty = "dotted", lwd = 0.4, color = "black")+
  geom_segment(aes(x = 67 , y = 5, xend = 67, yend = 0), lty = "dashed", lwd = 0.4, color = "grey60")+
  geom_segment(aes(x = 67 , y = 5.2, xend = 63, yend = 5.2), lty = "solid", lwd = 0.2, color = "black", 
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  annotate("text", x=80, y=5.3, label = "+4", size=2)+
  geom_segment(aes(x = 49 , y = 4.2, xend = 60, yend = 4.2), lty = "solid", lwd = 0.2, color = "black", #time
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  geom_segment(aes(x = 117, y = 4.2, xend = 73, yend = 4.2), lty = "solid", lwd = 0.2, color = "black", #time
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  annotate("text", x=55, y=4.3, label = "-18", size=2)+
  annotate("text", x=80, y=4.3, label = "+50", size=2)+
  geom_segment(aes(x = 3 , y = 3.2, xend = 60, yend = 3.2), lty = "solid", lwd = 0.2, color = "black", #space
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  geom_segment(aes(x = 181, y = 3.2, xend = 73, yend = 3.2), lty = "solid", lwd = 0.2, color = "black", #space
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  annotate("text", x=55, y=3.3, label = "-64", size=2)+
  annotate("text", x=80, y=3.3, label = "+114", size=2)+
  geom_segment(aes(x =  59, y = 2.2, xend = 60, yend = 2.2), lty = "solid", lwd = 0.2, color = "black", #parameter
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  geom_segment(aes(x = 75, y = 2.2, xend = 73, yend = 2.2), lty = "solid", lwd = 0.2, color = "black", #parameter
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  annotate("text", x=55, y=2.3, label = "-7", size=2)+
  annotate("text", x=80, y=2.3, label = "+8", size=2)+
  geom_segment(aes(x =  2, y = 1.2, xend = 60, yend = 1.2), lty = "solid", lwd = 0.2, color = "black", #model
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  geom_segment(aes(x = 286, y = 1.2, xend = 73, yend = 1.2), lty = "solid", lwd = 0.2, color = "black", #model
               arrow = grid::arrow(length = unit(0.1,"cm")))+
  annotate("text", x=55, y=1.3, label = "-65", size=2)+
  annotate("text", x=80, y=1.3, label = "+219", size=2)+
  geom_errorbarh(aes(xmax = area_weighted_flux + upper_75, xmin = area_weighted_flux - lower_75, height = .35, color = `Variability Source`))+
  geom_point(pch = 21, size = 6, color = "black")+
  labs(x = expression(paste("Standardized Global CH"[4]," Flux (g CH"[4]," m"^-2," yr"^-1,")")), y = "Flux Estimate Variability Source")
  
p

ggsave("./figures/Global_rate_comparison.jpeg", device = "jpeg", dpi = 1000, width = 20, height = 16, units = "cm")
  
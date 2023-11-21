library(dplyr)
library(ggplot2)
# Clear your environment and memory
rm(list=ls())
gc()


belgium <- ne_states(country = "belgium", returnclass = "sf")%>%
  st_transform("+proj=eqearth +wktext") %>%
  select(wikipedia)

FLUX_ESTIAMTES <- read_csv("./output_data/belgium_combined_and_adjusted_prediction.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")
  

baseline <- FLUX_ESTIAMTES %>% select(geometry,`Baseline Estimate`) %>%
  filter(`Baseline Estimate` < quantile(.$`Baseline Estimate`, 0.95)) %>%
ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Baseline Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  Baseline")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                       name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  #coord_sf(xlim = c(-15000000, 16000000), ylim = c(-7000000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#### TEMPORAL MAPS #####

time_low <- FLUX_ESTIAMTES %>% select(geometry,`Time-Low Estimate`) %>%
  filter(`Time-Low Estimate` < quantile(.$`Time-Low Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Time-Low Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  Low-Temporal Flux Estiamtes")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                        name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  #coord_sf(xlim = c(-15000000, 16000000), ylim = c(-7000000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

time_high <- FLUX_ESTIAMTES %>% select(geometry,`Time-high Estimate`) %>%
  filter(`Time-high Estimate` < quantile(.$`Time-high Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Time-high Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  High-Temporal Flux Estiamtes")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                        name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  #coord_sf(xlim = c(-15000000, 16000000), ylim = c(-7000000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#### SPATIAL MAPS #####
space_low <- FLUX_ESTIAMTES %>% select(geometry,`Space-Low Estimate`) %>%
  filter(`Space-Low Estimate` < quantile(.$`Space-Low Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Space-Low Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  Low-Spatial Flux Estiamtes")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                        name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  #coord_sf(xlim = c(-15000000, 16000000), ylim = c(-7000000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

space_high <- FLUX_ESTIAMTES %>% select(geometry,`Space-High Estimate`) %>%
  filter(`Space-High Estimate` < quantile(.$`Space-High Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Space-High Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  High-Spatial Flux Estiamtes")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                        name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  #coord_sf(xlim = c(-15000000, 16000000), ylim = c(-7000000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#### PARAMETER MAPS #####

param_low <- FLUX_ESTIAMTES %>% select(geometry,`Param-Low Estimate`) %>%
  filter(`Param-Low Estimate` < quantile(.$`Param-Low Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Param-Low Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  -1 SD Parameters")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                       name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

param_high <- FLUX_ESTIAMTES %>% select(geometry,`Param-High Estimate`) %>%
  filter(`Param-High Estimate` < quantile(.$`Param-High Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Param-High Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  +1 SD Parameters")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                       name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#### MODEL MAPS #####
model_low <- FLUX_ESTIAMTES %>% select(geometry,`Model-Low Estimate`) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Model-Low Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  -1 Residual Standard Deviation")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                        name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

model_high <- FLUX_ESTIAMTES %>% select(geometry,`Model-High Estimate`) %>%
  filter(`Model-High Estimate` < quantile(.$`Model-High Estimate`, 0.95)) %>%
  ggplot(.) +
  geom_sf(lwd = 0.05, pch=16,size = 4,
          aes(color = `Model-High Estimate`))+
  geom_sf(data = belgium, lwd =0.5, color = "black", fill = NA)+
  #geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  labs(title = "  +1 Residual Standard Deviation")+
  scale_color_gradient2(low="blue",
                        mid = "green",
                        high="red", midpoint = 625, space ="Lab", na.value="black",
                        name = "**Standardized CH<sub>4</sub> Flux** <br> g CH<sub>4</sub> m<sup>-2</sup> yr<sup>-1</sup>", limits = c(0,1250)) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=13),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#### COMBINE PLOTS AND SAVE ####
global_plot <- (baseline + plot_spacer())/
        (time_low + time_high)/
        (space_low + space_high)/
        (param_low + param_high)/
        (model_low + model_high)+ 
  plot_layout(guides = 'collect')

ggsave(plot=global_plot, './figures/big_map.jpg', width=12, height=20, units='in', dpi = 700)

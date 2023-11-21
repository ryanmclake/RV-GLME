# Clear your environment and memory
rm(list=ls())
gc()

# load in the GLEE data with the lat lon coordinates
map <- read_csv("./source_data/GLEE_data_with_GLCP_link.csv")

## --> Download the globe from R Natural Earth and exclude Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf", continent = c("africa","north america", "south america", "asia", "europe", "oceania"))

## --> Assign the flux pathway to the lakes and reservoirs
k <- map %>%
  #select the columns we need
  select(lat, lon, waterbody_type, ch4_ebu, ch4_diff) %>%
  # group by geographic location and waterbody type
  group_by(lat, lon, waterbody_type) %>%
  # summarize to single values for each site
  summarize_all(funs(mean)) %>%
  # assign an Emission type
  mutate(`Emission` = ifelse(!is.na(ch4_ebu),"Both Fluxes", "Diffusion"),
         `Emission` = ifelse(is.na(ch4_diff),"Ebullition", `Emission`)) %>%
  # convert geographic coordinates to sf objects
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") %>%
  # select only lakes and reservoirs... There are some goofy rivers and beaver ponds
  filter(waterbody_type == "lake" | waterbody_type == "reservoir") %>%
  # Assign a more reliable name for the column for the map below
  rename(`Waterbody Type` = waterbody_type)

## --> Make a global plot
map <- ggplot() +
  geom_sf(data = world, lwd = 0.3, color = "black", fill = "grey90")+
  xlab("Longitude") + ylab("Latitude") +
  geom_sf(data = k, size = 2, aes(shape = Emission, color = `Waterbody Type`))+
  scale_color_manual(values = c("blue", "orange"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.5),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

# Save the figure in the figures folder
ggsave("./figures/site_map.jpeg", device = "jpeg", dpi = 1000, width = 8, height = 5, units = "in")

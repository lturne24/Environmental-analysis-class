
#Overlaying metal and HNLC data 

#metals and eco region and nutrient group 
ggplot() +
  # base map
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  # metals (background layer)
  geom_point(data = metals_low,
             aes(x = lon.Clean, y = lat.Clean),
             color = "black",
             size = 0.5,
             alpha = 0.4) +
  
  # chlorophyll sites (foreground layer)
  geom_point(data = byECO9_HNLChlorophyll_sites,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = EcoRegion,
                 shape = nutrient_group),
             size = 1.8) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "HNLC Chlorophyll Sites Overlaid with Low Metal Sites",
       subtitle = "Black = metals < threshold, Colored = chlorophyll sites",
       color = "EcoRegion",
       shape = "Nutrient Group") +
  theme(legend.position = "right")


#just metals and nutrient group 
ggplot() +
  # base map
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  # metals (colored by metal)
  geom_point(data = metals_low,
             aes(x = lon.Clean, y = lat.Clean, color = Metal),
             size = 0.8,
             alpha = 0.7) +
  
  # chlorophyll sites (shaped by nutrient group)
  geom_point(data = byECO9_HNLChlorophyll_sites,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 shape = nutrient_group),
             color = "black",
             size = 1) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = paste0("HNLC Sites and Metals <  µg/L"),
       color = "Metal",
       shape = "Nutrient Group") +
  theme(legend.position = "right")

#Where is overlaps ----
####figure out how to make into like circle and see if overlap that way ----
metals_low2 <- metals_low %>%
  mutate(
    lat_round = round(lat.Clean, 3),
    lon_round = round(lon.Clean, 3)
  )

chloro2 <- byECO9_HNLChlorophyll_sites %>%
  mutate(
    lat_round = round(LAT_DD83, 3),
    lon_round = round(LON_DD83, 3)
  )

overlap_sites <- metals_low2 %>%
  inner_join(chloro2,
             by = c("lat_round", "lon_round"))

overlap_sites_clean <- overlap_sites %>%
  distinct(MonitoringLocationIdentifier, Metal, .keep_all = TRUE)

View(overlap_sites_clean)

summary(overlap_sites_clean$Metal)


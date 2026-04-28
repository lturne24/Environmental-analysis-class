
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
#overlap by coorindates ----
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

#overlap by COMID ----

overlap_COMID <- metals_low %>%
  inner_join(byECO9_HNLChlorophyll_sites, by = "COMID")


#Overlap by HUC 8 watershed 
overlap_huc8_Clean <- overlap_huc8 %>%
  group_by(MonitoringLocationIdentifier,
           lat.Clean,
           lon.Clean,
           nutrient_group) %>%
  
  summarise(
    metals_present = paste(sort(unique(Metal)), collapse = ", "),
    .groups = "drop"
  )


#overlap by distance ----
metals_sf <- st_as_sf(metals_low,
                      coords = c("lon.Clean", "lat.Clean"),
                      crs = 4326)

chloro_sf <- st_as_sf(byECO9_HNLChlorophyll_sites,
                      coords = c("LON_DD83", "LAT_DD83"),
                      crs = 4326)

# transform to projected CRS for distance (meters)
metals_sf <- st_transform(metals_sf, 5070)
chloro_sf <- st_transform(chloro_sf, 5070)

# join within 1 km
overlap_1km_Distance <- st_join(
  metals_sf,
  chloro_sf,
  join = st_is_within_distance,
  dist = 1000)

overlap_500m_Distance <- st_join(
  metals_sf,
  chloro_sf,
  join = st_is_within_distance,
  dist = 500)


#mapping overlap 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = overlap_huc8,
             aes(x = lon.Clean,
                 y = lat.Clean,
                 color = Metal,
                 shape = nutrient_group),
             size = 1,
             alpha = 0.95) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Overlap HUC8 Sites: Metals + Nutrients",
       color = "Metal",
       shape = "Nutrient Group") +
  theme(legend.position = "right")

#mapping overlap 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = overlap_huc8_Clean,
             aes(x = lon.Clean,
                 y = lat.Clean,
                 color = metals_present,
                 shape = nutrient_group),
             size = 1.5,
             alpha = 0.95) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Overlap HUC8 Sites: Metals + Nutrients",
       color = "Metal Limited",
       shape = "Nutrient Group") +
  theme(legend.position = "right")





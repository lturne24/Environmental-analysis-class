m.both <- leaflet() %>% # not using the dataset as a parameter here - why not???
  setView(lng = -77.34, lat = 38.65, zoom = 10) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addPolygons(data = parks, popup = ~NAME, label = ~NAME, color = "green") %>%
  addPolylines(data = portage_streams, color = "blue", 
               popup = ~paste0(FULLNAME, ": ", LINEARID))

m.both




sa2 <- counties_joined %>%
  filter(NAME %in% c("Hamilton", "Clermont")) %>% st_make_valid()

sa2 <- sa2 %>%
  mutate(pop_density = poptotal / (ALAND / 1e6))  # people per km²

places_sa <- st_filter(places, sa2)

places_pop <- places_sa %>%
  st_join(sa2[, c("pop_density")])

parks_all <- read_sf("./static_mapping/oh_parks.gpkg") %>%
  st_transform(st_crs(sa2)) %>% st_intersection(sa2) %>% st_make_valid()

big_parks <- parks_all

map_task5 <- 
  tm_shape(places_pop) +
  tm_polygons(
    fill = "pop_density",
    fill.scale = tm_scale_intervals(n = 5, values = "Reds"),
    fill.legend = tm_legend(title = "Population Density")
  ) +
  tm_borders(col = "black") +
  
  tm_shape(parks_all) +
  tm_polygons(
    fill = "darkgreen",
    fill_alpha = 0.5
  ) +
  
  tm_title("Population Density by Municipality with Park Distribution") +
  tm_scalebar() +
  tm_compass()

map_task5
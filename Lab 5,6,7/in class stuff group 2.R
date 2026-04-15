# Base map centers on the cheaspeake bay and shows counties 
G2 <- leaflet() %>%
  setView(lng = -77.3, lat = 38.6, zoom = 7) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
  addPolygons(
    data = counties,
    color = "black",
    weight = 1,
    fill = FALSE,
    group = "Counties")%>%
 ##adds drainage area  
  addCircleMarkers(
    data = stations,
    radius = ~sqrt(Drainage_A) / 10, 
    fillColor = "blue",
    fillOpacity = 0.5,
    color = "blue",
    stroke = TRUE,
    weight = 1,
    group = "Monitoring Stations",
    popup = ~paste("Drainage Area:", Drainage_A)) %>%
  #year dams removed
addCircleMarkers(
  data = dams,
  radius = 5,
  fillColor = ~colorNumeric(
    palette = "YlOrRd",
    domain = dams$DamRemoval
  )(DamRemoval),
  fillOpacity = 0.8,
  color = "black",
  weight = 1,
  group = "Dam Removals",
  popup = ~paste("Year Removed:", DamRemoval)) %>%
  #make so toggle layers 
addLayersControl(
  baseGroups = c("NatGeo"),
  overlayGroups = c("Counties", "Monitoring Stations", "Dam Removals"),
  options = layersControlOptions(collapsed = FALSE))



G2

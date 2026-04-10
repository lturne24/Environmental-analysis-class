#ok

library(sf)
library(dplyr)
library(tmap)

#--------------------------------------------------
# 1. Load data
#--------------------------------------------------

counties <- st_read("~/Documents/GitHub/Environmental-analysis-class/EA_Labs/data/static_mapping/oh_counties.gpkg")
places   <- st_read("~/Documents/GitHub/Environmental-analysis-class/EA_Labs/data/static_mapping/oh_places.gpkg")
parks    <- st_read("~/Documents/GitHub/Environmental-analysis-class/EA_Labs/data/static_mapping/oh_parks.gpkg")

# linear water (two separate folders → combine)
water_portage <- st_read("~/Documents/GitHub/Environmental-analysis-class/EA_Labs/data/static_mapping/tl_2022_39133_linearwater/")
water_summit  <- st_read("~/Documents/GitHub/Environmental-analysis-class/EA_Labs/data/static_mapping/tl_2023_39153_linearwater/")

# Check CRS
st_crs(counties)
st_crs(parks)
st_crs(places)
places <- st_transform(places, st_crs(counties))
parks  <- st_transform(parks,  st_crs(counties))
water_portage <- st_transform(water_portage, st_crs(counties))
water_summit  <- st_transform(water_summit,  st_crs(counties))

sf::sf_use_s2(FALSE)

# Fix geometries
counties <- st_make_valid(counties)
places   <- st_make_valid(places)
parks    <- st_make_valid(parks)
water_portage <- st_make_valid(water_portage)
water_summit  <- st_make_valid(water_summit)
#--------------------------------------------------
# 2. Filter to Portage + Summit counties
#--------------------------------------------------
target_counties <- counties %>%
  filter(NAME %in% c("Portage", "Summit"))

#--------------------------------------------------
# 3. Crop all layers to those counties
#--------------------------------------------------
places_local <- st_intersection(places, target_counties)
parks_local  <- st_intersection(parks, target_counties)

# combine water first, THEN crop
water_all <- bind_rows(water_portage, water_summit)
water_local <- st_intersection(water_all, target_counties)

#--------------------------------------------------
# 4. Identify streams that intersect parks
#--------------------------------------------------
water_local$intersects_park <- lengths(
  st_intersects(water_local, parks_local)
) > 0

#--------------------------------------------------
# 5. Build the map
#--------------------------------------------------
tmap_mode("plot")

map_local <- 
  tm_shape(target_counties) +
  tm_borders(col = "black", lwd = 2) +
  
  # Parks (shades of green by FEATTYPE)
  tm_shape(parks_local) +
  tm_polygons("FEATTYPE",
              palette = "Greens",
              alpha = 0.7,
              title = "Park Type") +
  
  # Water (highlight those intersecting parks)
  tm_shape(water_local) +
  tm_lines(col = "intersects_park",
           palette = c("gray60", "blue"),
           lwd = 1.5,
           title = "Intersects Park") +
  
  # Municipal boundaries + labels
  tm_shape(places_local) +
  tm_borders(col = "gray40") +
  tm_text("NAME",
          size = 0.6,
          col = "black") +
  
  tm_layout(title = "Portage & Summit Counties: Local Features")

map_local

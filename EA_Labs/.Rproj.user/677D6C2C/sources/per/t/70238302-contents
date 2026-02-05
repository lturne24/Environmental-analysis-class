#lab 1 

library(tidyverse)
library(sf)
library(ggplot2)



p.counties <- "./CBW/County_Boundaries.shp"
p.stations <-"./CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
 

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

glimpse(d.counties)
glimpse (d.stations)

#check Validity 
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()

d.counties <- d.counties %>% sf::st_is_valid()


d.counties %>% dplyr:: select(GEOID10, ALAND10) %>% head()



# next class week 4 thursday 

library(tidyverse)
library(sf)
library(ggplot2)

p.counties <- "./CBW/County_Boundaries.shp"
p.stations <-"./CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

glimpse(d.counties)
glimpse (d.stations)


del.counties <- d.counties %>% dplyr:: filter(STATEFP10 == 10)

d.counties %>% sf :: st_crs()
d.stations %>% sf :: st_crs()


#making sure that the CRS coorindate reference system is the same for both files should come back true
d.counties %>% sf:: st_crs() == d.stations %>% sf :: st_crs()

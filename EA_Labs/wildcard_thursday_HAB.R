#Wildcard thursday 
install.packages("rmarkdown")


library(tidyverse)
library(sf)
library(ggplot2)


#reading the data
bb_miss <- "./wildcard_thursday/bb_miss.shp"
lake_C <-"./wildcard_thursday/lake_champlain.shp"
lake_s <- "./wildcard_thursday/lake_stations.shp"

bb_miss <- sf::read_sf(bb_miss)
lake_C <- sf::read_sf(lake_C)
lake_s <- sf::read_sf(lake_s)

glimpse(bb_miss)








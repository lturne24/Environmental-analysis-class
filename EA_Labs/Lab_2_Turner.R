#Lab 2

library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)

#reading the data

counties <- sf::read_sf("./CBW/County_Boundaries.shp") %>% sf::st_make_valid()

dams <- sf::read_sf("./CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()

streams <- sf::read_sf("./CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid()

bmps <- read_csv("./CBW/BMPreport2016_landbmps.csv")

glimpse(d.counties)
glimpse(d.dams)
glimpse(d.blockage)
glimpse(bmps)



counties <- sf::read_sf("./CBW/County_Boundaries.shp") %>% sf::st_make_valid()

dams <- sf::read_sf("./CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()

streams <- sf::read_sf("./CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid()

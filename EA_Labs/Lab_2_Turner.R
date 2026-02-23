#Lab 2

library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)

#reading the data
p.counties <- "./CBW/County_Boundaries.shp"
p.dams <-"./CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp"
p.blockage <- "./CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp"

d.counties <- sf::read_sf(p.counties)
d.dams <- sf::read_sf(p.dams)
d.blockage <- sf::read_sf(p.blockage)
bmps <- read_csv("./CBW/BMPreport2016_landbmps.csv")

glimpse(d.counties)
glimpse(d.dams)
glimpse(d.blockage)
glimpse(bmps)




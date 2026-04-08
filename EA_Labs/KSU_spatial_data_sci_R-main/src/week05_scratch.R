# wk 5 scratch

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(tmap)




oh_counties <- read_sf("./data/ohio/oh_counties.gpkg")

oh_parks_shp <- read_sf("./data/ohio/ohio_parks.shp")
oh_parks <- read_sf("./data/ohio/oh_parks.gpkg")



oh_streams <- read_sf("./data/ohio/ohio_rivers.gpkg")
tm_shape(oh_streams) + tm_lines()



oh_counties <- read_sf("./data/ohio/oh_counties.gpkg")
counties_areas <- oh_counties %>% sf::st_area()


pc <- oh_counties %>% dplyr::filter(., NAME == "Portage")


pc_streams <- sf::st_intersection(oh_streams, pc)

oh_streams <- read_sf("./ohio/ohio_rivers.gpkg")

oh_streams %>% sf::st_crs()


tm_shape(oh_streams) + tm_lines()

oh_counties <- read_sf("./ohio/oh_counties.gpkg")
oh_counties %>% glimpse()


counties_areas <- oh_counties %>% sf::st_area()

portage <- oh_counties %>% dplyr::filter(., NAME == "Portage")


portage %>% tm_shape(.) + tm_polygons()

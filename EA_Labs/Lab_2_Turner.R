#Lab 2
#load data and libraries ----
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(RColorBrewer)

#reading the data
counties <- sf::read_sf("./CBW/County_Boundaries.shp") %>% sf::st_make_valid()
dams <- sf::read_sf("./CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()
streams <- sf::read_sf("./CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid()
bmps <- read_csv("./CBW/BMPreport2016_landbmps.csv")

glimpse(counties)
glimpse(dams)
glimpse(streams)
glimpse(bmps)

#Task 1 ----

#question 1.1
Q1.1 <- bmps %>% group_by(StateAbbreviation) %>%
  summarise(
    mean = mean(Cost, na.rm = TRUE),
    median = median(Cost, na.rm = TRUE),
    minRange = min(Cost, na.rm = TRUE),
    maxRange = max(Cost, na.rm = TRUE),
    standardDev = sd(Cost, na.rm = TRUE),
  )
Q1.1

#question 1.2
bmps_acres <- bmps %>% filter(Unit == "Acres")

P1.2 <- ggplot(bmps_acres, aes(x = TotalAmountCredited, y = Cost)) +
  geom_point(alpha = 0.2) +
  scale_y_log10() + scale_x_log10() + theme_minimal()
P1.2

#question 1.3
covercrop <- bmps %>% filter(str_detect(BMP, regex("cover crop", ignore_case = TRUE)))
#go back and change ratios to make prettier 
P1.3 <- ggplot(covercrop, aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation)) + theme_linedraw() +
  theme(aspect.ratio = 2)
P1.3

#question 1.4 
dams_filtered <- dams %>% filter(YEAR != 0)

P1.4 <- ggplot(dams_filtered, aes(x = YEAR, y = STATE)) +
  geom_point() + theme_minimal()
P1.4

#question 1.5
bmps <- bmps %>% mutate(FIPS = str_sub(GeographyName, 1, 5))
counties_joined <- counties %>% left_join(bmps, by = c("GEOID" = "FIPS"))

P1.5 <- bmps %>%
  group_by(StateAbbreviation, Sector) %>%
  summarise(totalCost = sum(Cost, na.rm = TRUE)) %>%
  ggplot(aes(x = StateAbbreviation, y = totalCost, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_linedraw() + scale_fill_brewer(palette = "Set3")
P1.5

#Task 2 ----

#question 2.1
Q2.1 <- streams %>% arrange(desc(LengthKM)) %>% slice(1:5)
Q2.1

#question 2.2
streams_counties <- st_intersection(streams, counties)

Q2.2 <- streams_counties %>%
  st_drop_geometry() %>%
  group_by(GEOID10) %>%
  summarise(totalLength = sum(LengthKM, na.rm = TRUE)) %>%
  arrange(desc(totalLength)) %>%
  slice(1:3)
Q2.2

#question 2.3
bmps <- bmps %>% mutate(FIPS = str_sub(GeographyName, 1, 5))
bmps_county <- bmps %>% group_by(FIPS) %>%
  summarise(totalCost = sum(Cost, na.rm = TRUE))
counties_bmps <- counties %>% left_join(bmps_county, by = c("GEOID10" = "FIPS"))
M2.3 <- tm_shape(counties_bmps) + tm_polygons("totalCost")
M2.3


#question 2.4
nearest_stream_index <- st_nearest_feature(dams, streams)
dams_nearest <- dams %>% mutate(nearest_stream_comid = streams$ComID[nearest_stream_index])
dams_nearest <- dams %>% mutate(nearest_stream_comid = streams$ComID[nearest_stream_index])
distances <- st_distance(dams, streams[nearest_stream_index, ], by_element = TRUE)
dams_nearest <- dams_nearest %>% mutate(distance_to_stream = as.numeric(distances))
Q2.4 <- dams_nearest %>% st_drop_geometry() %>%
  select(DAM_NAME, STATE, nearest_stream_comid, distance_to_stream)
Q2.4

#question 2.5 
Q2.5 <- dams %>% group_by(STATE) %>% summarise(n_dams = n())
Q2.5

#Answers ----
Q1.1 #question 1.1
P1.2 #question 1.2
P1.3 #question 1.3
P1.4 #question 1.4
P1.5 #question 1.5
Q2.1 #question 2.1
Q2.2 #question 2.2
M2.3 #question 2.3
Q2.4 #question 2.4
Q2.5 #question 2.5



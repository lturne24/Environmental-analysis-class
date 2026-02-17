#lab 1 

library(tidyverse)
library(sf)
library(ggplot2)


#reading the data
p.counties <- "./CBW/County_Boundaries.shp"
p.stations <-"./CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
 
d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

#look at data
glimpse(d.counties)
glimpse (d.stations)

#making sure coorindates line up 
d.counties %>% sf::st_is_valid()
d.stations %>% sf::st_is_valid()

d.counties <- d.counties %>% sf::st_make_valid()

#Task 1 ----

#question 1 
Q1_plw <- d.counties %>% group_by(STATEFP10) %>%
  mutate(
    TstateA = sum(ALAND10 + AWATER10),
    countyTA = ALAND10 + AWATER10,
    landPstate = (ALAND10 / TstateA) * 100
  )

Q1_plw

#Question 2
Q2_waterPcounty <- d.counties %>%
  mutate(waterP = AWATER10 / (ALAND10 + AWATER10)) %>% group_by(STATEFP10) %>%
  slice_max(waterP, n = 1)

Q2_waterPcounty

#Question 3
Q3_Countcounty <- d.counties %>% as_tibble() %>% 
  count(STATEFP10, name = "countiesNumbers")

Q3_Countcounty


#Question 4
Q4_shortStation <- d.stations %>%
  mutate(nameLength = nchar(STATION_NA)) %>%
  slice_min(nameLength, n = 1)

Q4_shortStation 


#Task 2 ----

#Question 1
P1 <- ggplot ( d.counties, aes (x = ALAND10, y = AWATER10, color = STATEFP10)) +
  geom_point(alpha = 0.6, size = 1) +
  labs(
    title = "Land Area v Water Area by County",
    x = "Land Area",
    y = "Water Area",
    color = "State"
  )
P1

#Question 2

P2 <- ggplot(d.stations, aes(x = Drainage_A)) +
  geom_histogram(bins = 25) +
  labs(
    title = "Drainage Area for Monitoring Stations",
    x = "Drainage Area",
    y = "Count"
  )
P2

#Question 3

stateStations <- st_intersection(d.stations, d.counties)

P3 <- ggplot(stateStations, aes(x = Drainage_A, fill = STATEFP10)) +
  geom_histogram(bins = 25, alpha = 0.6) +
  labs(
    title = "Drainage Area by State",
    x = "Drainage Area",
    y = "Count",
    fill = "State"
  )
P3


#Task 3 ----

#Question 1 

T3Q1 <- function(x) {
  if (!is.numeric(x)) {
    stop("womp womp not a number womp womp")
  }
  stats <- list(
    mean = mean(x),
    median = median(x),
    max = max(x),
    min = min(x),
    sorted = sort(x)
  )
  return(stats)
}

T3Q1(c(1, 0, -1))
T3Q1(c(10, 1000, 100))
T3Q1(c(.1, .001, 1e8))
T3Q1(c("a", "b", "c"))

#Task 4 ----
#Question 1

StationinState <- st_intersection(d.stations, d.counties) %>%
  as_tibble() %>%
  count(STATEFP10, name = "n_stations")

StationinState

#question 2
NewYORKKKK <- d.counties %>%
  filter(STATEFP10 == "36") %>% mutate(area = st_area(.)) %>%
  summarise(mean_area = mean(area))

NewYORKKKK


#Question 3
avgDrainState <- st_intersection(d.stations, d.counties) %>%
  as_tibble() %>%
  group_by(STATEFP10) %>%
  summarise(mean_drainage = mean(Drainage_A, na.rm = TRUE)) %>%
  slice_max(mean_drainage, n = 1)

avgDrainState 

#Questions ----
#1  
del.counties <- d.counties %>% dplyr:: filter(STATEFP10 == 10)

sf::st_intersection(d.stations, del.counties)
sf::st_intersection(del.counties, d.stations)

Print (" no they our not the one with d.stations first is a complete agruement but the 2nd one
       with del.counties first R wants you to add more to the code to make a complete command
       addtionally d.stations is a point and del.counties is a shape so the first ask where 
       our these points located in this shape but the second is just a shape with points 
       inside it")

#2 
print("i found that using github was challenging because of locked file issue 
      but once that was resolved it was much easier, additionally i was struggling to 
      understand what some of the headers did, is there a metadata file or 
      should i just know what each of these headers means ") 
#3
print (" i would like to see mapping on R and different statsically test your can do with maps,
       most of my past research has been lab based not field based so i would like
       to gain experience in working with more field based data/ location data.i also would
       like more things to organize my code better like with the 4 dashes, i didnt know that and 
       it was super helpful")


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


rainfall <- c(0.0, 2.1, 2.5, .1, 0.0, 0.0, 3.6, 4.0, 0.2, 4.0)

#check what is in the first position 
rainfall[1]

#check if its great then 2
rainfall[1] >= 3


if (rainfall[1] >= 3){
  print ("big storm")
} else {
  print ("litte storm")
}


#goes throught and tells whether a storm is big or little 
#makes a loop ----

f.storm.test <- function(rainfallAMT) {
  if (rainfallAMT >= 3){
    print ("big storm")
  } else {
    print ("litte storm")
  }
}

for(i in rainfall){
  f.storm.test(i)
}


#loops our inefficient 

#purrr is more effeicent 
rainfall %>% purrr::map(., f.storm.test)


#vectorized operations 

### if(sometest) { do some test here ex. >=3}

# figure out which day has the highest rainfall 
max(rainfall)

which(rainfall == max(rainfall))


mydf <- read_csv("./ne_counties.csv")


mydf %>% dplyr::slice_max(MedValHous)

#find median of each county is less then the max
newdf <- mydf %>% mutate(deviation = MedValHous - max(MedValHous))

newdf %>% dplyr::slice_min(deviation)

#make a historgram 
newdf %>% ggplot(., aes(x = deviation)) +
  geom_histogram() +
  theme_minimal()

#makea nice historgram 
newdf %>% ggplot(., aes(x = deviation)) +
  geom_histogram(fill = "purple") +
  theme_classic() +
  labs(title = "Deviations from maximum NE housing value",
       subtitle = "County scale",
       x = "Deviation",
       y = "Count")

#go crazyyyyyy

newdf %>% ggplot(., aes(x = deviation, y = after_stat(density))) +
  geom_histogram(fill = "dark green") +
  geom_vline(xintercept = mean(newdf$deviation), color = "red", linewidth = 2) +
  geom_density(color = "black", linewidth = 1) +
  theme_classic() +
  labs(title = "Deviations from maximum NE housing value",
       subtitle = "County scale",
       x = "Deviation",
       y = "Density")




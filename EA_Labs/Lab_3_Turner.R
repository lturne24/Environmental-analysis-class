#lab 3


#load librarys ----
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(spdep)
library(tidycensus)
library (tigris)

#load in data ----
options(tigris_use_cache = TRUE) # cache geometry so you don't dl unnecessarily
d.all <- get_acs(geography = "county", table = "B15003",
                 output = "wide", geometry = T, year = 2020)
glimpse(d.all)

df.vars <- load_variables(2020, "acs5", cache = T)
glimpse(df.vars)

d.sex.by.age <- get_acs(geography = "county", table = "B01001",
      output = "wide", geometry = T, year = 2020)
glimpse(d.sex.by.age)
states_sf <- states(cb = TRUE) 

#task 1 ----
states <- c("OH", "IN", "MI", "PA")
data <- get_acs( geography = "county", table = "B01001",
  year = 2020, geometry = TRUE, output = "wide")
# OH, IN, MI, PA -> 39, 18, 26, 42
data_subset <- data %>%
  mutate(STATE = substr(GEOID, 1, 2)) %>%
  filter(STATE %in% c("39", "18", "26", "42"))  

#task 2 choose varaible ----
data_subset <- data_subset %>%
  mutate(pct_f75_79 = B01001_047E / B01001_001E * 100)

# task 3 histogram of variable ----
ggplot(data_subset, aes(x = pct_f75_79)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black", linewidth = 0.3) +
  theme_minimal() + labs(title = "Percent Females 75–79")

#task 4 chlorpleth map ----
map_mode("plot")
tm_shape(data_subset) +
  tm_polygons( "pct_f75_79", style = "quantile",
    palette = "Greens", title = "% Female 75–79") +
  tm_shape(states_sf) + 
  tm_borders(col = "black", lwd = 2)

#task 5----
##5.1 
data_proj <- st_transform(data_subset, "ESRI:102010")
nb <- poly2nb(data_proj, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

##task 5.2
neighbors <- attr(lw$weights, "comp")$d
hist(neighbors, main = "Neighbor Count")

##task 5.3 
mean(neighbors)

#task 5.4
moran.test(data_proj$pct_f75_79, lw)
moran.plot(data_proj$pct_f75_79, lw, zero.policy = TRUE)








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
states <- states(cb = TRUE, year = 2020)


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
  geom_histogram(bins = 30, fill = "darkgreen", color = "black", linewidth = 0.4) +
  theme_minimal() + labs(title = "Percent Females 75–79")

#task 4 chlorpleth map ----
tmap_mode("plot")
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

#task 6
coords <- st_centroid(data_proj)
coords_mat <- st_coordinates(coords)
nb_dist <- dnearneigh(coords_mat, 0, 200000)  
# about 200 km
lw_dist <- nb2listw(nb_dist, style = "W", zero.policy = TRUE)

neighbors_dist <- attr(lw_dist$weights, "comp")$d
hist(neighbors_dist)
mean(neighbors_dist)
moran.test(data_proj$pct_f75_79, lw_dist)
moran.plot(data_proj$pct_f75_79, lw_dist, zero.policy = TRUE)

#Questions ----
#Question 1
# Morans I is spatial correlations by looking at the neighbors 
#and comparing the value of a variable at a location to their neighbors 
#It can have positive autocorrelation (similar values near each other) 
#or negative auto correlation (similar value our dispersed)"

#question 2 
#It is the average of a variables value from the neighbors 
#so how much influence is has on nearby observations in a location 

#question 3
#In task 5 (contiguity) it defines neighbors based on shared boarders
#but in 6 (IDW) defines based on distance from so the changes in which 
#location causes influence 

#question 4
#The high-Low observations means it has a high value surrounded by low 
#values, It can show you outliers spatially and unusual patterns

#bonus----

#B1
x <- scale(data_proj$pct_f75_79)[,1]
lag_x <- lag.listw(lw, x)
local_moran <- localmoran(x, lw)

moran_df <- data.frame(
  x = x, lag_x = lag_x,
  p_value = local_moran[,5])

moran_df <- moran_df %>%
  mutate(
    quadrant = case_when(
      x > 0 & lag_x > 0 ~ "HH",
      x < 0 & lag_x < 0 ~ "LL",
      x > 0 & lag_x < 0 ~ "HL",
      x < 0 & lag_x > 0 ~ "LH"),
    significant = ifelse(p_value < 0.05, "Yes", "No"))

ggplot(moran_df, aes(x = x, y = lag_x)) +
  geom_point(aes(color = quadrant, shape = significant), size = 1.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs( title = "Manual Moran Plot", x = "Standardized Value",
    y = "Spatial Lag") + theme_minimal() + 
  annotate("text", x = 2, y = 2, label = "HH") +
  annotate("text", x = -2, y = -2, label = "LL") +
  annotate("text", x = 2, y = -2, label = "HL") +
  annotate("text", x = -2, y = 2, label = "LH")




#B2 
data_proj$quadrant <- moran_df$quadrant
data_proj$significant <- moran_df$significant
data_proj <- data_proj %>%
  mutate(
    lisa_cluster = ifelse(significant == "Yes", quadrant, "Not Significant")
  )

states <- states(cb = TRUE, year = 2020)
states_subset <- states %>%
  filter(STUSPS %in% c("OH", "IN", "MI", "PA"))

data_proj$lisa_cluster <- factor( data_proj$lisa_cluster,
  levels = c("HH", "LL", "HL", "LH", "Not Significant"))

#HH= red, LL = Purple, HL= orange, LH= blue, NS= gray 

tm_shape(data_proj) +
  tm_polygons( "lisa_cluster", palette = c("red","purple",     
      "orange","blue","grey95"     
      ), title = "LISA Clusters") + tm_shape(states_subset) +
  tm_borders(col = "black", lwd = 1.75)

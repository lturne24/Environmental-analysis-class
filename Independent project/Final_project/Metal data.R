#starting over but organized 
#load Librarys ----
library(tidyverse)
library(sf)
library(terra)
library(fs)
library(lubridate)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(maps)
library(patchwork)
library(readr)
library(dplyr)
library(writexl)
library(openxlsx)

#load in data ----
Iron <- read.csv("./data/Iron_aggregated_high_quality_start2000_cleaned.csv")
Manganese <- read.csv("./data/Manganese_aggregated_high_quality_start2000_cleaned.csv")
Nickel <- read.csv("./data/Nickel_aggregated_high_quality_start2000_cleaned.csv")
Zinc <- read_csv("data/Zinc_aggregated_high_quality_start2000_cleaned.csv")
us_states <- map_data("state")


#shorten data (2018 to 2019 only)
names(Iron)
view(Iron$ActivityStartDate)

Iron_2018_2019 <- Iron %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate)) %>%
  filter(year(ActivityStartDate) %in% c(2018, 2019))

Manganese_2018_2019 <- Manganese %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate)) %>%
  filter(year(ActivityStartDate) %in% c(2018, 2019))

Nickel_2018_2019 <- Nickel %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate)) %>%
  filter(year(ActivityStartDate) %in% c(2018, 2019))

Zinc_2018_2019 <- Zinc %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate)) %>%
  filter(year(ActivityStartDate) %in% c(2018, 2019))

summary(Iron_2018_2019$DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL)
summary(Manganese_2018_2019$DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL)
summary(Nickel_2018_2019$DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL)
summary(Zinc_2018_2019$DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL)
#filter out low metals (below 10ugL)----

Iron_low <- Iron_2018_2019 %>%
  filter(DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL < 10)

Manganese_low <- Manganese_2018_2019 %>%
  filter(DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL < 1)

Nickel_low <- Nickel_2018_2019 %>%
  filter(DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL < 1.4)

Zinc_low <- Zinc_2018_2019 %>%
  filter(DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL < 5)

#combine low metals into one set ----
Iron_low$Metal <- "Iron"
Manganese_low$Metal <- "Manganese"
Nickel_low$Metal <- "Nickel"
Zinc_low$Metal <- "Zinc"

Iron_low$HUCEightDigitCode <- as.character(Iron_low$HUCEightDigitCode)
Manganese_low$HUCEightDigitCode <- as.character(Manganese_low$HUCEightDigitCode)
Nickel_low$HUCEightDigitCode <- as.character(Nickel_low$HUCEightDigitCode)
Zinc_low$HUCEightDigitCode <- as.character(Zinc_low$HUCEightDigitCode)

metals_low <- dplyr::bind_rows(
  Iron_low,
  Manganese_low,
  Nickel_low,
  Zinc_low)


#map all them metals ----
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = metals_low,
             aes(x = lon.Clean, y = lat.Clean, color = Metal),
             size = .5,
             alpha = 0.7) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(color = "Metal",
       title = paste0("Sites with Metals <  µg/L")) +
  theme(legend.position = "bottom")


#map each metal indivdually ----
##Iron ----
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = Iron_low,
             aes(x = lon.Clean, y = lat.Clean),
             color = "red",
             size = 0.7,
             alpha = 0.7) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Iron < 10 µg/L") +
  theme(legend.position = "none")


##manganese ----
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = Manganese_low,
             aes(x = lon.Clean, y = lat.Clean),
             color = "blue",
             size = 0.7,
             alpha = 0.7) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Manganese < 10 µg/L") +
  theme(legend.position = "none")

##Nickel ----
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = Nickel_low,
             aes(x = lon.Clean, y = lat.Clean),
             color = "green",
             size = 0.7,
             alpha = 0.7) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Nickel < 10 µg/L") +
  theme(legend.position = "none")

##Zinc ----

ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = Zinc_low,
             aes(x = lon.Clean, y = lat.Clean),
             color = "purple",
             size = 0.7,
             alpha = 0.7) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Zinc < 10 µg/L") +
  theme(legend.position = "none")










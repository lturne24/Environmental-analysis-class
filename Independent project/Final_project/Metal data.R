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
view(Iron$)


names(Zinc)
view(Zinc$ActivityStartDate)

boxplot(Iron$)


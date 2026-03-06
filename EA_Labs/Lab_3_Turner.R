#lab 3


#load librarys ----
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(spdep)
library(tidycensus)

#load in data ----
options(tigris_use_cache = TRUE) # cache geometry so you don't dl unnecessarily
d.all <- get_acs(geography = "county", table = "B15003",
                 output = "wide", geometry = T, year = 2020)
glimpse(d.all)

df.vars <- load_variables(2020, "acs5", cache = T)
glimpse(df.vars)






# load in packages ----
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

#load in data ----
waterchem <- read.csv("./data/River_stream_18_19_water_chemistry_data.csv")
periphyton <- read.csv ("./data/River_stream_18_19_water_periphyton_biomass.csv")
chlorophyll <- read.csv("./data/River_stream_18_19_water_chlorophyll_a.csv")
SiteInfo <- read.csv("./data/River_stream_18_19_siteInfo_data.csv")



#only pulls nutrients out 
nutrients <- waterchem %>%
  select(
    SITE_ID, VISIT_NO,
    AMMONIA_N_RESULT,
    NITRATE_N_RESULT,
    NITRITE_N_RESULT,
    NITRATE_NITRITE_N_RESULT,
    TKN_RESULT,
    NTL_RESULT,
    NTL_DISS_RESULT,
    PTL_RESULT,
    PTL_DISS_RESULT,
    DOC_RESULT,
    TSS_RESULT,
    TURB_RESULT
  )

#general summary of stuff 
nutrients %>%
  select(-SITE_ID, -VISIT_NO) %>%
  summarise(across(everything(), list(
    min = ~min(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )))


#look at N:P ratios 
nutrients <- nutrients %>%
  mutate(NP_ratio = NTL_RESULT / PTL_RESULT)
summary(nutrients$NP_ratio)
hist(nutrients$NP_ratio, breaks = 50)

#attach data 
chl_clean <- chlorophyll %>%
  select(SITE_ID, VISIT_NO, CHL = RESULT)

full_data <- nutrients %>%
  left_join(chl_clean, by = c("SITE_ID", "VISIT_NO"))
us_states <- map_data("state")

# data loading ----
names(waterchem)
names (periphyton)
names(chlorophyll)
names (SiteInfo)
names (Landscape)

#sites with high nutrient (PandN) but low C
HNLC_sites <- combined_spatial %>%
  filter(HNLC_strict == 1) %>%
  distinct(SITE_ID, STATE_NM, US_L3NAME, LAT_DD83, LON_DD83)

HNLC_sites
nrow(HNLC_sites)

#high P low c only sites
HNLC_P_only <- combined_spatial %>%
  filter(HNLC_stat == 1) %>%
  distinct(SITE_ID, STATE_NM, US_L3NAME)
HNLC_P_only

nrow(HNLC_P_only)


#just data exploring stuff----
dim(waterchem)
dim(chlorophyll)
dim(periphyton)
dim(SiteInfo)

#number of different sites 
length(unique(waterchem$SITE_ID))
length(unique(periphyton$SITE_ID))
length(unique(chlorophyll$SITE_ID))
length(unique(SiteInfo$SITE_ID))

#phosphorus distrubution 
summary(waterchem$PTL_RESULT)
hist(waterchem$PTL_RESULT, breaks = 50)
unique(waterchem$PTL_UNITS)
#is right skewed HEAVILY

#chlorphy 
summary(chlorophyll$RESULT)
hist(chlorophyll$RESULT, breaks = 50)
unique(chlorophyll$RESULT_UNITS)
#right skewed but not alot 


#missing results
sum(is.na(waterchem$PTL_RESULT))
sum(is.na(chlorophyll$RESULT))

phos <- waterchem %>% select(SITE_ID, VISIT_NO, PTL_RESULT)
chl <- chlorophyll %>% select(SITE_ID, VISIT_NO, RESULT)
combined <- left_join(phos, chl, by = c("SITE_ID", "VISIT_NO"))
summary(combined)

#scatterplot looking at total P and chlorophyl
ggplot(combined, aes(x = PTL_RESULT, y = RESULT)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(x = "Total Phosphorus",
       y = "Chlorophyll-a")

combined <- combined %>% mutate(HNLC_candidate = ifelse(PTL_RESULT > 10 & RESULT < 5, 1, 0))
table(combined$HNLC_candidate)

combined_spatial <- combined %>%
  left_join(SiteInfo %>% select(SITE_ID, LAT_DD83, LON_DD83, STATE),
            by = "SITE_ID")

#ECOREGIONS ----

#figure out what level of ecoreigon to use 
#probably want to use AG_ECO9_NM or NA_L1NAME
HNLC_data %>% summarise(num_ecoregions = n_distinct(AG_ECO3_NM)) 
unique(HNLC_data$AG_ECO3_NM)

#THIS ONE
HNLC_data %>% summarise(num_ecoregions = n_distinct(AG_ECO9_NM))
unique(HNLC_data$AG_ECO9_NM)

HNLC_data %>% summarise(num_ecoregions = n_distinct(US_L4NAME))
unique(HNLC_data$US_L4NAME)

HNLC_data %>% summarise(num_ecoregions = n_distinct(US_L3NAME))
unique(HNLC_data$US_L3NAME)

HNLC_data %>% summarise(num_ecoregions = n_distinct(NA_L2NAME))
unique(HNLC_data$NA_L2NAME)


HNLC_data %>% summarise(num_ecoregions = n_distinct(NA_L1NAME))
unique(HNLC_data$NA_L1NAME)

HNLC_data %>% summarise(num_ecoregions = n_distinct(PERIPHYTON_RESULT))
unique(HNLC_data$PERIPHYTON_RESULT)


geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  geom_point(data = HNLC_data, 
             aes(x = LON_DD83, y = LAT_DD83, color = NA_L1NAME),
             size = 0.5, alpha = 0.9) +
  scale_color_brewer(palette = "Set1") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "HNLC Sites by EPA Eco 9 Regions",
       x = "Longitude",
       y = "Latitude",
       color = "Eco Region")





#ecoreiong map 

# Load required packages
library(ggplot2)
library(maps)
library(dplyr)

# Example: your HNLC_data should have at least columns: longitude, latitude, AG_ECO9
# If not, rename the columns to lon/lat
# HNLC_data <- rename(HNLC_data, lon = LONGITUDE_COL, lat = LATITUDE_COL)

# Get US map data
us_map <- map_data("state")

# Plot
ggplot() +
  # US states as background
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  # Plot your sites colored by ecoregion
  geom_point(data = HNLC_data, 
             aes(x = LON_DD83, y = LAT_DD83, color = AG_ECO9),
             size = 0.5, alpha = 0.9) +
  # Color scale
  scale_color_brewer(palette = "Set1") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "HNLC Sites by EPA Eco 9 Regions",
       x = "Longitude",
       y = "Latitude",
       color = "Eco Region")


#looking at the eco region by NA_L1
ggplot() +
  # US states as background
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  # Plot your sites colored by ecoregion
  geom_point(data = waterchem, 
             aes(x = LON_DD83, y = LAT_DD83, color = waterchem$MAGNESIUM_RESULT),
             size = 0.5, alpha = 0.9) +
  # Color scale
  scale_color_brewer(palette = "Set1") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "HNLC Sites by EPA Eco 9 Regions",
       x = "Longitude",
       y = "Latitude",
       color = "Eco Region")



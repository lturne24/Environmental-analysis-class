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

names(waterchem)
names (periphyton)
names(chlorophyll)
names (SiteInfo)



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






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

#load in data ----
waterchem <- read.csv("./data/River_stream_18_19_water_chemistry_data.csv")
periphyton <- read.csv ("./data/River_stream_18_19_water_periphyton_biomass.csv")
chlorophyll <- read.csv("./data/River_stream_18_19_water_chlorophyll_a.csv")
SiteInfo <- read.csv("./data/River_stream_18_19_siteInfo_data.csv")
Landscape <- read_csv("data/River_stream_18_19_Landscape.csv")


#clean up and put into a big data set ----
waterchem_clean <- waterchem %>%
  select(
    SITE_ID, VISIT_NO, DATE_COL,
    AMMONIA_N_RESULT, AMMONIA_N_UNITS,
    NITRATE_N_RESULT,NITRATE_N_UNITS,
    NITRITE_N_RESULT,NITRITE_N_UNITS,
    NITRATE_NITRITE_N_RESULT,NITRATE_NITRITE_N_UNITS,
    NTL_RESULT,NTL_UNITS,
    NTL_DISS_RESULT,NTL_DISS_UNITS,
    TKN_RESULT,TKN_UNITS,
    PTL_RESULT,PTL_UNITS,
    PTL_DISS_RESULT,PTL_DISS_UNITS
  )
chlorophyll_clean <- chlorophyll %>%
  select(
    SITE_ID, VISIT_NO,DATE_COL,
    CHLA_RESULT = RESULT, CHLA_UNITS = RESULT_UNITS
  )
periphyton_clean <- periphyton %>%
  select(
    SITE_ID, VISIT_NO, DATE_COL,
    ANALYTE, PERIPHYTON_RESULT = RESULT, RESULT_UNITS
  )
SiteInfo <- SiteInfo %>%
  mutate(
    VISIT_NO = as.character(VISIT_NO), DATE_COL = as.character(DATE_COL)
  )
waterchem_clean <- waterchem_clean %>%
  mutate(
    VISIT_NO = as.character(VISIT_NO), DATE_COL = as.character(DATE_COL)
  )
chlorophyll_clean <- chlorophyll_clean %>%
  mutate(
    VISIT_NO = as.character(VISIT_NO), DATE_COL = as.character(DATE_COL)
  )
periphyton_clean <- periphyton_clean %>%
  mutate(
    VISIT_NO = as.character(VISIT_NO),DATE_COL = as.character(DATE_COL)
  )
combined_data <- SiteInfo
combined_data <- combined_data %>% left_join(waterchem_clean, 
            by = c("SITE_ID","VISIT_NO","DATE_COL"))
combined_data <- combined_data %>% left_join(chlorophyll_clean,
            by = c("SITE_ID","VISIT_NO","DATE_COL"))
combined_data <- combined_data %>% left_join(periphyton_clean,
            by = c("SITE_ID","VISIT_NO","DATE_COL"))
#THE BIG DADDY OF DATA SETS ----
HNLC_data <- combined_data







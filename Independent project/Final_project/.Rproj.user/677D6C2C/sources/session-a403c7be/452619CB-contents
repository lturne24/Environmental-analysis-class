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

#load in data ----
waterchem <- read.csv("./data/River_stream_18_19_water_chemistry_data.csv")
periphyton <- read.csv ("./data/River_stream_18_19_water_periphyton_biomass.csv")
chlorophyll <- read.csv("./data/River_stream_18_19_water_chlorophyll_a.csv")
SiteInfo <- read.csv("./data/River_stream_18_19_siteInfo_data.csv")
Landscape <- read_csv("data/River_stream_18_19_Landscape.csv")
us_states <- map_data("state")

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

#determine sites with High nutrients low chlorophyll----

# Calculate thresholds (top quarlite of nutrients bottom of chlorophyll )
P75 <- quantile(HNLC_data$PTL_RESULT, 0.75, na.rm = TRUE)
N75 <- quantile(HNLC_data$NTL_RESULT, 0.75, na.rm = TRUE)
C25 <- quantile(HNLC_data$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25 <- quantile(HNLC_data$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)

#sites with low chlorophyll and perhpyton 
low_chl_sites <- HNLC_data %>% dplyr::filter(CHLA_RESULT <= C25)
low_per_sites <- HNLC_data %>% dplyr::filter(PERIPHYTON_RESULT <= PE25)


#shows sites with if they have high p / high n or both and only keeps HNLChlorrphyll or perphyton
HNLChlorophyll_sites <- low_chl_sites %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75 & NTL_RESULT >= N75 ~ "High N + High P",
      PTL_RESULT >= P75 & NTL_RESULT < N75 ~ "High P only",
      NTL_RESULT >= N75 & PTL_RESULT < P75 ~ "High N only",
      TRUE ~ NA_character_
    )
  )
HNLChlorophyll_sites <- HNLChlorophyll_sites %>%
  dplyr::filter(!is.na(nutrient_group))


HNLPerphyton_sites <- low_per_sites %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75 & NTL_RESULT >= N75 ~ "High N + High P",
      PTL_RESULT >= P75 & NTL_RESULT < N75 ~ "High P only",
      NTL_RESULT >= N75 & PTL_RESULT < P75 ~ "High N only",
      TRUE ~ NA_character_
    )
  )
HNLPerphyton_sites <- HNLPerphyton_sites %>%
  dplyr::filter(!is.na(nutrient_group))

#Mapping all data ----
#makes a simple list later for Mapping
HNLChlorophyll_sites_list <- HNLChlorophyll_sites %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list$nutrient_group)


HNLPerphyton_sites_list <- HNLPerphyton_sites %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT,NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPerphyton_sites_list$nutrient_group)

#HNLchlorphyll sites Map
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  theme_minimal() + 
  theme(
    legend.position = "bottom"
  )

#HNLPerphyton sites Map
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPerphyton_sites_list,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )



#ECOREGIONS AG_ECO9 ----

us_map <- map_data("state")
# Plot all sites according to eco region color AG_ECO9_NM
ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray95", color = "black",linewidth = 0.2) +
  geom_point(data = HNLC_data, 
             aes(x = LON_DD83, y = LAT_DD83, color = AG_ECO9_NM),
             size = 0.75, alpha = 0.9) +
  scale_color_brewer(palette = "Paired") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "HNLC Sites by EPA Eco 9 Regions",
       x = "Longitude",
       y = "Latitude",
       color = "Eco Region")

#figure out what level of ecoreigon to use 
HNLC_data %>% summarise(num_ecoregions = n_distinct(AG_ECO9_NM))
unique(HNLC_data$AG_ECO9_NM)
unique(HNLC_data$AG_ECO9)

#split into 9 ecoreigns 
SPL <- subset(HNLC_data, AG_ECO9 == "SPL") #southern plains 
TPL <- subset(HNLC_data, AG_ECO9 == "TPL") #temperate plains
CPL <- subset(HNLC_data, AG_ECO9 == "CPL") #Coastal Plains 
UMW <- subset(HNLC_data, AG_ECO9 == "UMW") #upper midwest
SAP <- subset(HNLC_data, AG_ECO9 == "SAP") #Southern Appalachians
NAP <- subset(HNLC_data, AG_ECO9 == "NAP") #Northern Appalachians
WMT <- subset(HNLC_data, AG_ECO9 == "WMT") #Western Mountains
NPL <- subset(HNLC_data, AG_ECO9 == "NPL") #Northern Plains
XER <- subset(HNLC_data, AG_ECO9 == "XER") #Xeric

#find high nutrients and low chrollyphl/ low perphyton for each ECOreign 9 ----


##Southern Plains SPL ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_SPL <- quantile(SPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_SPL <- quantile(SPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_SPL <- quantile(SPL$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_SPL <- quantile(SPL$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_SPL <- SPL %>% dplyr::filter(CHLA_RESULT <= C25_SPL)
low_per_sites_SPL <- SPL %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_SPL)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_SPL <- low_chl_sites_SPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_SPL & NTL_RESULT >= N75_SPL ~ "High N + High P",
      PTL_RESULT >= P75_SPL & NTL_RESULT < N75_SPL ~ "High P only",
      NTL_RESULT >= N75_SPL & PTL_RESULT < P75_SPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_SPL <- low_per_sites_SPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_SPL & NTL_RESULT >= N75_SPL ~ "High N + High P",
      PTL_RESULT >= P75_SPL & NTL_RESULT < N75_SPL ~ "High P only",
      NTL_RESULT >= N75_SPL & PTL_RESULT < P75_SPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))



## Temperate plains TPL ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_TPL <- quantile(TPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_TPL <- quantile(TPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_TPL <- quantile(TPL$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_TPL <- quantile(TPL$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_TPL <- TPL %>% dplyr::filter(CHLA_RESULT <= C25_TPL)
low_per_sites_TPL <- TPL %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_TPL)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_TPL <- low_chl_sites_TPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_TPL & NTL_RESULT >= N75_TPL ~ "High N + High P",
      PTL_RESULT >= P75_TPL & NTL_RESULT < N75_TPL ~ "High P only",
      NTL_RESULT >= N75_TPL & PTL_RESULT < P75_TPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_TPL <- low_per_sites_TPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_TPL & NTL_RESULT >= N75_TPL ~ "High N + High P",
      PTL_RESULT >= P75_TPL & NTL_RESULT < N75_TPL ~ "High P only",
      NTL_RESULT >= N75_TPL & PTL_RESULT < P75_TPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))



##Coastal plains CPL----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_CPL <- quantile(CPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_CPL <- quantile(CPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_CPL <- quantile(CPL$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_CPL <- quantile(CPL$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_CPL <- CPL %>% dplyr::filter(CHLA_RESULT <= C25_CPL)
low_per_sites_CPL <- CPL %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_CPL)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_CPL <- low_chl_sites_CPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_CPL & NTL_RESULT >= N75_CPL ~ "High N + High P",
      PTL_RESULT >= P75_CPL & NTL_RESULT < N75_CPL ~ "High P only",
      NTL_RESULT >= N75_CPL & PTL_RESULT < P75_CPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_CPL <- low_per_sites_CPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_CPL & NTL_RESULT >= N75_CPL ~ "High N + High P",
      PTL_RESULT >= P75_CPL & NTL_RESULT < N75_CPL ~ "High P only",
      NTL_RESULT >= N75_CPL & PTL_RESULT < P75_CPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))



##Upper Midwest UMW ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_UMW <- quantile(UMW$PTL_RESULT, 0.75, na.rm = TRUE)
N75_UMW <- quantile(UMW$NTL_RESULT, 0.75, na.rm = TRUE)
C25_UMW <- quantile(UMW$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_UMW <- quantile(UMW$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_UMW <- UMW %>% dplyr::filter(CHLA_RESULT <= C25_UMW)
low_per_sites_UMW <- UMW %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_UMW)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_UMW <- low_chl_sites_UMW %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_UMW & NTL_RESULT >= N75_UMW ~ "High N + High P",
      PTL_RESULT >= P75_UMW & NTL_RESULT < N75_UMW ~ "High P only",
      NTL_RESULT >= N75_UMW & PTL_RESULT < P75_UMW ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_UMW <- low_per_sites_UMW %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_UMW & NTL_RESULT >= N75_UMW ~ "High N + High P",
      PTL_RESULT >= P75_UMW & NTL_RESULT < N75_UMW ~ "High P only",
      NTL_RESULT >= N75_UMW & PTL_RESULT < P75_UMW ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))

##Southern Appalachians SAP ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_SAP <- quantile(SAP$PTL_RESULT, 0.75, na.rm = TRUE)
N75_SAP <- quantile(SAP$NTL_RESULT, 0.75, na.rm = TRUE)
C25_SAP <- quantile(SAP$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_SAP <- quantile(SAP$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_SAP <- SAP %>% dplyr::filter(CHLA_RESULT <= C25_SPL)
low_per_sites_SAP <- SAP %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_SPL)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_SAP <- low_chl_sites_SAP %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_SAP & NTL_RESULT >= N75_SAP ~ "High N + High P",
      PTL_RESULT >= P75_SAP & NTL_RESULT < N75_SAP ~ "High P only",
      NTL_RESULT >= N75_SAP & PTL_RESULT < P75_SAP ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_SAP <- low_per_sites_SAP %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_SAP & NTL_RESULT >= N75_SAP ~ "High N + High P",
      PTL_RESULT >= P75_SAP & NTL_RESULT < N75_SAP ~ "High P only",
      NTL_RESULT >= N75_SAP & PTL_RESULT < P75_SAP ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))



##Northern Appalachians NAP ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_NAP <- quantile(NAP$PTL_RESULT, 0.75, na.rm = TRUE)
N75_NAP <- quantile(NAP$NTL_RESULT, 0.75, na.rm = TRUE)
C25_NAP <- quantile(NAP$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_NAP <- quantile(NAP$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_NAP <- NAP %>% dplyr::filter(CHLA_RESULT <= C25_NAP)
low_per_sites_NAP <- NAP %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_NAP)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_NAP <- low_chl_sites_NAP %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_NAP & NTL_RESULT >= N75_NAP ~ "High N + High P",
      PTL_RESULT >= P75_NAP & NTL_RESULT < N75_NAP ~ "High P only",
      NTL_RESULT >= N75_NAP & PTL_RESULT < P75_NAP ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_NAP <- low_per_sites_NAP %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_NAP & NTL_RESULT >= N75_NAP ~ "High N + High P",
      PTL_RESULT >= P75_NAP & NTL_RESULT < N75_NAP ~ "High P only",
      NTL_RESULT >= N75_NAP & PTL_RESULT < P75_NAP ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))



##Western Mountains WMT----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_WMT <- quantile(WMT$PTL_RESULT, 0.75, na.rm = TRUE)
N75_WMT <- quantile(WMT$NTL_RESULT, 0.75, na.rm = TRUE)
C25_WMT <- quantile(WMT$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_WMT <- quantile(WMT$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_WMT <- WMT %>% dplyr::filter(CHLA_RESULT <= C25_WMT)
low_per_sites_WMT <- WMT %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_WMT)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_WMT <- low_chl_sites_WMT %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_WMT & NTL_RESULT >= N75_WMT ~ "High N + High P",
      PTL_RESULT >= P75_WMT & NTL_RESULT < N75_WMT ~ "High P only",
      NTL_RESULT >= N75_WMT & PTL_RESULT < P75_WMT ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_WMT <- low_per_sites_WMT %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_WMT & NTL_RESULT >= N75_WMT ~ "High N + High P",
      PTL_RESULT >= P75_WMT & NTL_RESULT < N75_WMT ~ "High P only",
      NTL_RESULT >= N75_WMT & PTL_RESULT < P75_WMT ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))


##Northern Plains NPL ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_NPL <- quantile(NPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_NPL <- quantile(NPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_NPL <- quantile(NPL$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_NPL <- quantile(NPL$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_NPL <- NPL %>% dplyr::filter(CHLA_RESULT <= C25_NPL)
low_per_sites_NPL <- NPL %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_NPL)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_NPL <- low_chl_sites_NPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_NPL & NTL_RESULT >= N75_NPL ~ "High N + High P",
      PTL_RESULT >= P75_NPL & NTL_RESULT < N75_NPL ~ "High P only",
      NTL_RESULT >= N75_NPL & PTL_RESULT < P75_NPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_NPL <- low_per_sites_NPL %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_NPL & NTL_RESULT >= N75_NPL ~ "High N + High P",
      PTL_RESULT >= P75_NPL & NTL_RESULT < N75_NPL ~ "High P only",
      NTL_RESULT >= N75_NPL & PTL_RESULT < P75_NPL ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))


##Xeric XER ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_XER <- quantile(XER$PTL_RESULT, 0.75, na.rm = TRUE)
N75_XER <- quantile(XER$NTL_RESULT, 0.75, na.rm = TRUE)
C25_XER <- quantile(XER$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_XER <- quantile(XER$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_XER <- XER %>% dplyr::filter(CHLA_RESULT <= C25_XER)
low_per_sites_XER <- XER %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_XER)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_XER <- low_chl_sites_XER %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_XER & NTL_RESULT >= N75_XER ~ "High N + High P",
      PTL_RESULT >= P75_XER & NTL_RESULT < N75_XER ~ "High P only",
      NTL_RESULT >= N75_XER & PTL_RESULT < P75_XER ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_XER <- low_per_sites_XER %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_XER & NTL_RESULT >= N75_XER ~ "High N + High P",
      PTL_RESULT >= P75_XER & NTL_RESULT < N75_XER ~ "High P only",
      NTL_RESULT >= N75_XER & PTL_RESULT < P75_XER ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))

#Mapping EcoRegions 9 ----

##southern plains SPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_SPL <- HNLChlorophyll_sites_SPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_SPL$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_SPL <- HNLPeriphyton_sites_SPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_SPL$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_SPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Southern Plains - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_SPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Southern Plains - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Temperate Plains TPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_TPL <- HNLChlorophyll_sites_TPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_TPL$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_TPL <- HNLPeriphyton_sites_TPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_TPL$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_TPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Temperate Plains - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_TPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Temperate Plains - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Coastal Plains CPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_CPL <- HNLChlorophyll_sites_CPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_CPL$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_CPL <- HNLPeriphyton_sites_CPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_CPL$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_CPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Coastal Plains - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_CPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Coastal Plains - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")

##Upper Midwest UMW ----
#Chlorophyll list 
HNLChlorophyll_sites_list_UMW <- HNLChlorophyll_sites_UMW %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_UMW$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_UMW <- HNLPeriphyton_sites_UMW %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_UMW$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_UMW,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Upper Midwest - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_UMW,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Upper Midwest - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Southern Appalachians SAP ----
#Chlorophyll list 
HNLChlorophyll_sites_list_SAP <- HNLChlorophyll_sites_SAP %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_SAP$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_SAP <- HNLPeriphyton_sites_SAP %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_SAP$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_SAP,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Southern Appalachians - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_SAP,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Southern Appalachians - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Northern Appalachians NAP ----
#Chlorophyll list 
HNLChlorophyll_sites_list_NAP <- HNLChlorophyll_sites_NAP %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_NAP$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_NAP <- HNLPeriphyton_sites_NAP %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_NAP$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_NAP,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Northern Appalachians - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_NAP,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Northern Appalachians - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Western Mountains WMT ----
#Chlorophyll list 
HNLChlorophyll_sites_list_WMT <- HNLChlorophyll_sites_WMT %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_WMT$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_WMT <- HNLPeriphyton_sites_WMT %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_WMT$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_WMT,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Western Mountains - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_WMT,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Western Mountains - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Northern Plains NPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_NPL <- HNLChlorophyll_sites_NPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_NPL$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_NPL <- HNLPeriphyton_sites_NPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_NPL$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_NPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Northern Plains - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_NPL,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Northern Plains - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##Xeric XER ----
#Chlorophyll list 
HNLChlorophyll_sites_list_XER <- HNLChlorophyll_sites_XER %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_XER$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_XER <- HNLPeriphyton_sites_XER %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_XER$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_XER,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Xeric - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_XER,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Xeric - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")

#list of all sites by eco region ----
#chlorophyll 
HNLChlorophyll_sites_list_SPL$EcoRegion <- "SPL"
HNLChlorophyll_sites_list_TPL$EcoRegion <- "TPL"
HNLChlorophyll_sites_list_CPL$EcoRegion <- "CPL"
HNLChlorophyll_sites_list_UMW$EcoRegion <- "UMW"
HNLChlorophyll_sites_list_SAP$EcoRegion <- "SAP"
HNLChlorophyll_sites_list_NAP$EcoRegion <- "NAP"
HNLChlorophyll_sites_list_WMT$EcoRegion <- "WMT"
HNLChlorophyll_sites_list_NPL$EcoRegion <- "NPL"
HNLChlorophyll_sites_list_XER$EcoRegion <- "XER"

byECO9_HNLChlorophyll_sites <- dplyr::bind_rows(
  HNLChlorophyll_sites_list_SPL,
  HNLChlorophyll_sites_list_TPL,
  HNLChlorophyll_sites_list_CPL,
  HNLChlorophyll_sites_list_UMW,
  HNLChlorophyll_sites_list_SAP,
  HNLChlorophyll_sites_list_NAP,
  HNLChlorophyll_sites_list_WMT,
  HNLChlorophyll_sites_list_NPL,
  HNLChlorophyll_sites_list_XER
)

#periphyton
HNLPeriphyton_sites_list_SPL$EcoRegion <- "SPL"
HNLPeriphyton_sites_list_TPL$EcoRegion <- "TPL"
HNLPeriphyton_sites_list_CPL$EcoRegion <- "CPL"
HNLPeriphyton_sites_list_UMW$EcoRegion <- "UMW"
HNLPeriphyton_sites_list_SAP$EcoRegion <- "SAP"
HNLPeriphyton_sites_list_NAP$EcoRegion <- "NAP"
HNLPeriphyton_sites_list_WMT$EcoRegion <- "WMT"
HNLPeriphyton_sites_list_NPL$EcoRegion <- "NPL"
HNLPeriphyton_sites_list_XER$EcoRegion <- "XER"

byECO9_HNLPeriphyton_sites <- dplyr::bind_rows(
  HNLPeriphyton_sites_list_SPL,
  HNLPeriphyton_sites_list_TPL,
  HNLPeriphyton_sites_list_CPL,
  HNLPeriphyton_sites_list_UMW,
  HNLPeriphyton_sites_list_SAP,
  HNLPeriphyton_sites_list_NAP,
  HNLPeriphyton_sites_list_WMT,
  HNLPeriphyton_sites_list_NPL,
  HNLPeriphyton_sites_list_XER
)

#Mapping by eco 9 region ----
#chlorophyll map by eco region 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = byECO9_HNLChlorophyll_sites,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = EcoRegion,
                 shape = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  theme_minimal() +
  ggtitle("HNLC Chlorophyll Sites Across EPA EcoRegions") +
  theme(legend.position = "right")

#periphyton map by ecoregion 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = byECO9_HNLPeriphyton_sites,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = EcoRegion,
                 shape = nutrient_group),
             size = 3) +
  coord_fixed(1.3) +
  theme_minimal() +
  ggtitle("HNLC Periphyton Sites Across EPA EcoRegions") +
  theme(legend.position = "right")

#Ecoregion AG_ECO3----
PLNLOW <- subset(HNLC_data, AG_ECO3 == "PLNLOW") #Plains and Lowlands
EHIGH <- subset(HNLC_data, AG_ECO3 == "EHIGH") #Eastern Highlands
WMTNS <- subset(HNLC_data, AG_ECO3 == "WMTNS") #West


HNLC_data %>% summarise(num_ecoregions = n_distinct(AG_ECO3_NM)) 
unique(HNLC_data$AG_ECO3_NM)
HNLC_data %>% summarise(num_ecoregions = n_distinct(AG_ECO3)) 
unique(HNLC_data$AG_ECO3)

# Plot all sites according to eco region color AG_ECO3_NM
ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray95", color = "black",linewidth = 0.2) +
  geom_point(data = HNLC_data, 
             aes(x = LON_DD83, y = LAT_DD83, color = AG_ECO3_NM),
             size = 0.75, alpha = 0.9) +
  scale_color_brewer(palette = "Set2") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "HNLC Sites by EPA Eco 3 Regions",
       x = "Longitude",
       y = "Latitude",
       color = "Eco Region")

#find high nutrients and low chrollyphl/ low perphyton for each ECOreign 3 ----
##Plains and Lowlands PLNLOW ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_PLNLOW <- quantile(PLNLOW$PTL_RESULT, 0.75, na.rm = TRUE)
N75_PLNLOW <- quantile(PLNLOW$NTL_RESULT, 0.75, na.rm = TRUE)
C25_PLNLOW <- quantile(PLNLOW$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_PLNLOW <- quantile(PLNLOW$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_PLNLOW <- PLNLOW %>% dplyr::filter(CHLA_RESULT <= C25_SPL)
low_per_sites_PLNLOW <- PLNLOW %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_SPL)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_PLNLOW <- low_chl_sites_PLNLOW %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_PLNLOW & NTL_RESULT >= N75_PLNLOW ~ "High N + High P",
      PTL_RESULT >= P75_PLNLOW & NTL_RESULT < N75_PLNLOW ~ "High P only",
      NTL_RESULT >= N75_PLNLOW & PTL_RESULT < P75_PLNLOW ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_PLNLOW <- low_per_sites_PLNLOW %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_PLNLOW & NTL_RESULT >= N75_PLNLOW ~ "High N + High P",
      PTL_RESULT >= P75_PLNLOW & NTL_RESULT < N75_PLNLOW ~ "High P only",
      NTL_RESULT >= N75_PLNLOW & PTL_RESULT < P75_PLNLOW ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))

##Eastern Highlands EHIGH ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_EHIGH <- quantile(EHIGH$PTL_RESULT, 0.75, na.rm = TRUE)
N75_EHIGH <- quantile(EHIGH$NTL_RESULT, 0.75, na.rm = TRUE)
C25_EHIGH <- quantile(EHIGH$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_EHIGH <- quantile(EHIGH$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_EHIGH <- EHIGH %>% dplyr::filter(CHLA_RESULT <= C25_EHIGH)
low_per_sites_EHIGH <- EHIGH %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_EHIGH)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_EHIGH <- low_chl_sites_EHIGH %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_EHIGH & NTL_RESULT >= N75_EHIGH ~ "High N + High P",
      PTL_RESULT >= P75_EHIGH & NTL_RESULT < N75_EHIGH ~ "High P only",
      NTL_RESULT >= N75_EHIGH & PTL_RESULT < P75_EHIGH ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_EHIGH <- low_per_sites_EHIGH %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_EHIGH & NTL_RESULT >= N75_EHIGH ~ "High N + High P",
      PTL_RESULT >= P75_EHIGH & NTL_RESULT < N75_EHIGH ~ "High P only",
      NTL_RESULT >= N75_EHIGH & PTL_RESULT < P75_EHIGH ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))

##West WMTNS ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_WMTNS <- quantile(WMTNS$PTL_RESULT, 0.75, na.rm = TRUE)
N75_WMTNS <- quantile(WMTNS$NTL_RESULT, 0.75, na.rm = TRUE)
C25_WMTNS <- quantile(WMTNS$CHLA_RESULT, 0.25, na.rm = TRUE)
PE25_WMTNS <- quantile(WMTNS$PERIPHYTON_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_WMTNS <- WMTNS %>% dplyr::filter(CHLA_RESULT <= C25_WMTNS)
low_per_sites_WMTNS <- WMTNS %>% dplyr::filter(PERIPHYTON_RESULT <= PE25_WMTNS)
# classify nutrient conditions for chlorophyll sites
HNLChlorophyll_sites_WMTNS <- low_chl_sites_WMTNS %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_WMTNS & NTL_RESULT >= N75_WMTNS ~ "High N + High P",
      PTL_RESULT >= P75_WMTNS & NTL_RESULT < N75_WMTNS ~ "High P only",
      NTL_RESULT >= N75_WMTNS & PTL_RESULT < P75_WMTNS ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))
# classify nutrient conditions for periphyton sites
HNLPeriphyton_sites_WMTNS <- low_per_sites_WMTNS %>%
  dplyr::mutate(
    nutrient_group = dplyr::case_when(
      PTL_RESULT >= P75_WMTNS & NTL_RESULT >= N75_WMTNS ~ "High N + High P",
      PTL_RESULT >= P75_WMTNS & NTL_RESULT < N75_WMTNS ~ "High P only",
      NTL_RESULT >= N75_WMTNS & PTL_RESULT < P75_WMTNS ~ "High N only",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(nutrient_group))

#Mapping EcoRegions 3 ----

##Plains and Lowlands PLNLOW ----
#Chlorophyll list 
HNLChlorophyll_sites_list_PLNLOW <- HNLChlorophyll_sites_PLNLOW %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_PLNLOW$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_PLNLOW <- HNLPeriphyton_sites_PLNLOW %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_PLNLOW$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_PLNLOW,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Plains and Lowlands - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_PLNLOW,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Plains and Lowlands - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")

##Eastern Highlands EHIGH ----
#Chlorophyll list 
HNLChlorophyll_sites_list_EHIGH <- HNLChlorophyll_sites_EHIGH %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_EHIGH$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_EHIGH <- HNLPeriphyton_sites_PLNLOW %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_EHIGH$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_EHIGH,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Eastern Highlands - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_EHIGH,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("Eastern Highlands - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")


##West WMTNS ----
#Chlorophyll list 
HNLChlorophyll_sites_list_WMTNS <- HNLChlorophyll_sites_WMTNS %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_WMTNS$nutrient_group)
#periphyton list 
HNLPeriphyton_sites_list_WMTNS <- HNLPeriphyton_sites_WMTNS %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLPeriphyton_sites_list_WMTNS$nutrient_group)

#chlorphyll map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list_WMTNS,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("West - HNLC Chlorophyll Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")
#periphyton Map 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLPeriphyton_sites_list_WMTNS,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  ggtitle("West - HNLC Periphyton Sites") +
  theme_minimal() +
  theme(legend.position = "bottom")

#list of all sites by eco region 3----
#chlorophyll 
HNLChlorophyll_sites_list_PLNLOW$ECO3 <- "PLNLOW"
HNLChlorophyll_sites_list_EHIGH$ECO3 <- "EHIGH"
HNLChlorophyll_sites_list_WMTNS$ECO3 <- "WMTNS"

byECO3_HNLChlorophyll_sites <- dplyr::bind_rows(
  HNLChlorophyll_sites_list_PLNLOW,
  HNLChlorophyll_sites_list_EHIGH,
  HNLChlorophyll_sites_list_WMTNS
)

#Periphyton 
byECO3_HNLPeriphyton_sites <- dplyr::bind_rows(
  dplyr::mutate(HNLPeriphyton_sites_list_PLNLOW, ECO3 = "PLNLOW"),
  dplyr::mutate(HNLPeriphyton_sites_list_EHIGH, ECO3 = "EHIGH"),
  dplyr::mutate(HNLPeriphyton_sites_list_WMTNS, ECO3 = "WMTNS")
)

#Mapping by eco 3 region ----
#chlorophyll map by eco region 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = byECO3_HNLChlorophyll_sites,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = ECO3,
                 shape = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  theme_minimal() +
  ggtitle("HNLC Chlorophyll Sites Across EPA EcoRegions") +
  theme(legend.position = "right")

#periphyton map by ecoregion 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = byECO3_HNLPeriphyton_sites,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = ECO3,
                 shape = nutrient_group),
             size = 1) +
  coord_fixed(1.3) +
  theme_minimal() +
  ggtitle("HNLC Periphyton Sites Across EPA EcoRegions") +
  theme(legend.position = "right")



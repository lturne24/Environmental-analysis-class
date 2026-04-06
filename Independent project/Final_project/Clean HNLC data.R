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
waterchem <- read.csv("./data/River_stream_18_19_water_chemistry_data.csv")
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
combined_data <- SiteInfo
combined_data <- combined_data %>% left_join(waterchem_clean, 
                                             by = c("SITE_ID","VISIT_NO","DATE_COL"))
combined_data <- combined_data %>% left_join(chlorophyll_clean,
                                             by = c("SITE_ID","VISIT_NO","DATE_COL"))
#THE BIG DADDY OF DATA SETS ----
HNLC_data <- combined_data

#removes negative values (not possible)
HNLC_data <- HNLC_data %>%
  mutate( PTL_RESULT = ifelse(!is.na(PTL_RESULT) & PTL_RESULT < 0, 0, PTL_RESULT),
    PTL_DISS_RESULT = ifelse(!is.na(PTL_DISS_RESULT) & PTL_DISS_RESULT < 0, 0, PTL_DISS_RESULT))


#just mapping the sites (all of them) ----
#chlorophyll map (all Sites)
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  
  geom_point(data = HNLC_data,
             aes(x = LON_DD83, y = LAT_DD83, color = CHLA_RESULT),
             size = 1) +
  
  scale_color_gradient(
    low = "lightgreen",
    high = "darkgreen",
    na.value = "red"
  ) +
  
  coord_fixed(1.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


#determine sites with High nutrients low chlorophyll----

# Calculate thresholds (top quarlite of nutrients bottom of chlorophyll )
P75 <- quantile(HNLC_data$PTL_RESULT, 0.75, na.rm = TRUE)
N75 <- quantile(HNLC_data$NTL_RESULT, 0.75, na.rm = TRUE)
C25 <- quantile(HNLC_data$CHLA_RESULT, 0.25, na.rm = TRUE)

#sites with low chlorophyll and perhpyton 
low_chl_sites <- HNLC_data %>% dplyr::filter(CHLA_RESULT <= C25)

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

#Mapping all data ----
#makes a simple list later for Mapping
HNLChlorophyll_sites_list <- HNLChlorophyll_sites %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list$nutrient_group)


#HNLchlorphyll sites Map
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = HNLChlorophyll_sites_list,
             aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group),
             size = 1.5) +
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
# sites with low chlorophyll and periphyton
low_chl_sites_SPL <- SPL %>% dplyr::filter(CHLA_RESULT <= C25_SPL)
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


## Temperate plains TPL ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_TPL <- quantile(TPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_TPL <- quantile(TPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_TPL <- quantile(TPL$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_TPL <- TPL %>% dplyr::filter(CHLA_RESULT <= C25_TPL)

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

##Coastal plains CPL----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_CPL <- quantile(CPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_CPL <- quantile(CPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_CPL <- quantile(CPL$CHLA_RESULT, 0.25, na.rm = TRUE)
# sites with low chlorophyll and periphyton
low_chl_sites_CPL <- CPL %>% dplyr::filter(CHLA_RESULT <= C25_CPL)

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




##Upper Midwest UMW ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_UMW <- quantile(UMW$PTL_RESULT, 0.75, na.rm = TRUE)
N75_UMW <- quantile(UMW$NTL_RESULT, 0.75, na.rm = TRUE)
C25_UMW <- quantile(UMW$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_UMW <- UMW %>% dplyr::filter(CHLA_RESULT <= C25_UMW)

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

##Southern Appalachians SAP ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_SAP <- quantile(SAP$PTL_RESULT, 0.75, na.rm = TRUE)
N75_SAP <- quantile(SAP$NTL_RESULT, 0.75, na.rm = TRUE)
C25_SAP <- quantile(SAP$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_SAP <- SAP %>% dplyr::filter(CHLA_RESULT <= C25_SPL)

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


##Northern Appalachians NAP ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_NAP <- quantile(NAP$PTL_RESULT, 0.75, na.rm = TRUE)
N75_NAP <- quantile(NAP$NTL_RESULT, 0.75, na.rm = TRUE)
C25_NAP <- quantile(NAP$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_NAP <- NAP %>% dplyr::filter(CHLA_RESULT <= C25_NAP)

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


##Western Mountains WMT----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_WMT <- quantile(WMT$PTL_RESULT, 0.75, na.rm = TRUE)
N75_WMT <- quantile(WMT$NTL_RESULT, 0.75, na.rm = TRUE)
C25_WMT <- quantile(WMT$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_WMT <- WMT %>% dplyr::filter(CHLA_RESULT <= C25_WMT)

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


##Northern Plains NPL ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_NPL <- quantile(NPL$PTL_RESULT, 0.75, na.rm = TRUE)
N75_NPL <- quantile(NPL$NTL_RESULT, 0.75, na.rm = TRUE)
C25_NPL <- quantile(NPL$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_NPL <- NPL %>% dplyr::filter(CHLA_RESULT <= C25_NPL)

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



##Xeric XER ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll/periphyton)
P75_XER <- quantile(XER$PTL_RESULT, 0.75, na.rm = TRUE)
N75_XER <- quantile(XER$NTL_RESULT, 0.75, na.rm = TRUE)
C25_XER <- quantile(XER$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll and periphyton
low_chl_sites_XER <- XER %>% dplyr::filter(CHLA_RESULT <= C25_XER)

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



##Temperate Plains TPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_TPL <- HNLChlorophyll_sites_TPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_TPL$nutrient_group)

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



##Coastal Plains CPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_CPL <- HNLChlorophyll_sites_CPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_CPL$nutrient_group)

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

##Upper Midwest UMW ----
#Chlorophyll list 
HNLChlorophyll_sites_list_UMW <- HNLChlorophyll_sites_UMW %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_UMW$nutrient_group)

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

##Southern Appalachians SAP ----
#Chlorophyll list 
HNLChlorophyll_sites_list_SAP <- HNLChlorophyll_sites_SAP %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_SAP$nutrient_group)

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


##Northern Appalachians NAP ----
#Chlorophyll list 
HNLChlorophyll_sites_list_NAP <- HNLChlorophyll_sites_NAP %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_NAP$nutrient_group)

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

##Western Mountains WMT ----
#Chlorophyll list 
HNLChlorophyll_sites_list_WMT <- HNLChlorophyll_sites_WMT %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_WMT$nutrient_group)
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

##Northern Plains NPL ----
#Chlorophyll list 
HNLChlorophyll_sites_list_NPL <- HNLChlorophyll_sites_NPL %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_NPL$nutrient_group)

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

##Xeric XER ----
#Chlorophyll list 
HNLChlorophyll_sites_list_XER <- HNLChlorophyll_sites_XER %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_XER$nutrient_group)

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
             size = 1.5) +
  coord_fixed(1.3) +
  theme_minimal() +
  ggtitle("HNLC Chlorophyll Sites Across EPA Eco 9 Regions") +
  theme(legend.position = "right")


#Ecoregion AG_ECO3----
PLNLOW <- subset(HNLC_data, AG_ECO3 == "PLNLOW") #Plains and Lowlands
EHIGH <- subset(HNLC_data, AG_ECO3 == "EHIGH") #Eastern Highlands
WMTNS <- subset(HNLC_data, AG_ECO3 == "WMTNS") #West


HNLC_data %>% summarise(num_ecoregions = n_distinct(AG_ECO3_NM)) 
unique(HNLC_data$AG_ECO3_NM)

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

# sites with low chlorophyll
low_chl_sites_PLNLOW <- PLNLOW %>% dplyr::filter(CHLA_RESULT <= C25_SPL)

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


##Eastern Highlands EHIGH ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll)
P75_EHIGH <- quantile(EHIGH$PTL_RESULT, 0.75, na.rm = TRUE)
N75_EHIGH <- quantile(EHIGH$NTL_RESULT, 0.75, na.rm = TRUE)
C25_EHIGH <- quantile(EHIGH$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll
low_chl_sites_EHIGH <- EHIGH %>% dplyr::filter(CHLA_RESULT <= C25_EHIGH)

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

##West WMTNS ----
# determine sites with High nutrients low chlorophyll
# Calculate thresholds (top quartile nutrients, bottom quartile chlorophyll)
P75_WMTNS <- quantile(WMTNS$PTL_RESULT, 0.75, na.rm = TRUE)
N75_WMTNS <- quantile(WMTNS$NTL_RESULT, 0.75, na.rm = TRUE)
C25_WMTNS <- quantile(WMTNS$CHLA_RESULT, 0.25, na.rm = TRUE)

# sites with low chlorophyll
low_chl_sites_WMTNS <- WMTNS %>% dplyr::filter(CHLA_RESULT <= C25_WMTNS)

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

##Eastern Highlands EHIGH ----
#Chlorophyll list 
HNLChlorophyll_sites_list_EHIGH <- HNLChlorophyll_sites_EHIGH %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_EHIGH$nutrient_group)

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

##West WMTNS ----
#Chlorophyll list 
HNLChlorophyll_sites_list_WMTNS <- HNLChlorophyll_sites_WMTNS %>%
  dplyr::select(
    SITE_ID, LAT_DD83, LON_DD83,
    PTL_RESULT, NTL_RESULT, CHLA_RESULT,
    nutrient_group
  )
table(HNLChlorophyll_sites_list_WMTNS$nutrient_group)

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
             size = 1.5) +
  coord_fixed(1.3) +
  theme_minimal() +
  ggtitle("HNLC Chlorophyll Sites Across EPA Eco 3 Regions") +
  theme(legend.position = "right")

#List of Sites for download ----
write_xlsx(
  list(All_USA = HNLChlorophyll_sites_list,
    ECO9 = byECO9_HNLChlorophyll_sites,
    ECO3 = byECO3_HNLChlorophyll_sites
  ),"HNLChlorophyll_sites.xlsx")

#Box and violin plots ----

##all of USA  box plots ----
ggplot(data = HNLC_data, aes(x = "", y = CHLA_RESULT)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Chlorophyll results ",
       x = "all usa") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = "", y = PTL_RESULT)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Phorosurpos results ",
       x = "all usa") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = "", y = NTL_RESULT)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "nitrogen results ",
       x = "all usa") +
  theme_minimal()+
  theme(legend.position = "none")

#Just boxplot 
ggplot(data = HNLC_data, aes(x = "", y = CHLA_RESULT)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  labs(title = "Chlorophyll Results",
       y = "Chlorophyll",
       x = "All USA") +
  theme_minimal() +
  theme(legend.position = "none", #legend bye 
        aspect.ratio = 2) #make taller 


ggplot(data = HNLC_data, aes(x = "", y = PTL_RESULT)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  labs(title = "Phosphorus Results",
       y = "Phosphorus",
       x = "All USA") +
  theme_minimal() +
  theme(legend.position = "none", 
        aspect.ratio = 2)


ggplot(data = HNLC_data, aes(x = "", y = NTL_RESULT)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  labs(title = "Nitrogen Results",
       y = "Nitrogen",
       x = "All USA") +
  theme_minimal() +
  theme(legend.position = "none", 
        aspect.ratio = 2)

##eco 3 regions box plots----

#box and violing plots 
ggplot(data = HNLC_data, aes(x = AG_ECO3, y = CHLA_RESULT, fill = AG_ECO3)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Chlorophyll Results",
       x = "ECO 3 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO3, y = PTL_RESULT, fill = AG_ECO3)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Phosphorus Results",
       x = "ECO 3 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO3, y = NTL_RESULT, fill = AG_ECO3)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Nitrogen Results",
       x = "ECO 3 Region") +
  theme_minimal()+
  theme(legend.position = "none")

#Just box plots 
ggplot(data = HNLC_data, aes(x = AG_ECO3, y = CHLA_RESULT, fill = AG_ECO3)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Chlorophyll Box Plot",
       y = "Chlorophyll  Results",
       x = "ECO 3 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO3, y = PTL_RESULT, fill = AG_ECO3)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Phosphorus Box Plot ",
       y = "Phosphorus Results",
       x = "ECO 3 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO3, y = NTL_RESULT, fill = AG_ECO3)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Nitrogen Box Plot",
       y = "Nitrogen Results",
       x = "ECO 3 Region") +
  theme_minimal()+
  theme(legend.position = "none")

#eco 9 region boxplots ----
#box and violing plots 

ggplot(data = HNLC_data, aes(x = AG_ECO9, y = CHLA_RESULT, fill = AG_ECO9)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Chlorophyll Results",
       x = "ECO 9 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO9, y = PTL_RESULT, fill = AG_ECO9)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Phosphorus Results",
       x = "ECO 9 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO9, y = NTL_RESULT, fill = AG_ECO9)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Violin Plot with Box Plot Overlay",
       y = "Nitrogen Results",
       x = "ECO 9 Region") +
  theme_minimal()+
  theme(legend.position = "none")

#Just Box plots 

ggplot(data = HNLC_data, aes(x = AG_ECO9, y = CHLA_RESULT, fill = AG_ECO9)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Chlorophyll Box Plot",
       y = "Chlorophyll  Results",
       x = "ECO 9 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO9, y = PTL_RESULT, fill = AG_ECO9)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Phosphorus Box Plot ",
       y = "Phosphorus Results",
       x = "ECO 9 Region") +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = HNLC_data, aes(x = AG_ECO9, y = NTL_RESULT, fill = AG_ECO9)) +
  geom_boxplot(width = 0.2,
               outlier.size = 0.5,      #smaller dots
               outlier.alpha = 0.8) + #transparent
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Nitrogen Box Plot",
       y = "Nitrogen Results",
       x = "ECO 9 Region") +
  theme_minimal()+
  theme(legend.position = "none")

#summary table ----
get_summary <- function(x) {
  data.frame(
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
}

##Chlorphyll ----
CHLA_summary <- dplyr::bind_rows(
  "USA" = get_summary(HNLC_data$CHLA_RESULT),
  "PLNLOW" = get_summary(PLNLOW$CHLA_RESULT),
  "EHIGH"  = get_summary(EHIGH$CHLA_RESULT),
  "WMTNS"  = get_summary(WMTNS$CHLA_RESULT),
  "SPL" = get_summary(SPL$CHLA_RESULT),
  "TPL" = get_summary(TPL$CHLA_RESULT),
  "CPL" = get_summary(CPL$CHLA_RESULT),
  "UMW" = get_summary(UMW$CHLA_RESULT),
  "SAP" = get_summary(SAP$CHLA_RESULT),
  "NAP" = get_summary(NAP$CHLA_RESULT),
  "WMT" = get_summary(WMT$CHLA_RESULT),
  "NPL" = get_summary(NPL$CHLA_RESULT),
  "XER" = get_summary(XER$CHLA_RESULT),
  .id = "Region")

##phosphorus ----
PTL_summary <- dplyr::bind_rows(
  "USA" = get_summary(HNLC_data$PTL_RESULT),
  "PLNLOW" = get_summary(PLNLOW$PTL_RESULT),
  "EHIGH"  = get_summary(EHIGH$PTL_RESULT),
  "WMTNS"  = get_summary(WMTNS$PTL_RESULT),
  "SPL" = get_summary(SPL$PTL_RESULT),
  "TPL" = get_summary(TPL$PTL_RESULT),
  "CPL" = get_summary(CPL$PTL_RESULT),
  "UMW" = get_summary(UMW$PTL_RESULT),
  "SAP" = get_summary(SAP$PTL_RESULT),
  "NAP" = get_summary(NAP$PTL_RESULT),
  "WMT" = get_summary(WMT$PTL_RESULT),
  "NPL" = get_summary(NPL$PTL_RESULT),
  "XER" = get_summary(XER$PTL_RESULT),
  .id = "Region")

##nitrogen ----
NTL_summary <- dplyr::bind_rows(
  "USA" = get_summary(HNLC_data$NTL_RESULT),
  "PLNLOW" = get_summary(PLNLOW$NTL_RESULT),
  "EHIGH"  = get_summary(EHIGH$NTL_RESULT),
  "WMTNS"  = get_summary(WMTNS$NTL_RESULT),
  "SPL" = get_summary(SPL$NTL_RESULT),
  "TPL" = get_summary(TPL$NTL_RESULT),
  "CPL" = get_summary(CPL$NTL_RESULT),
  "UMW" = get_summary(UMW$NTL_RESULT),
  "SAP" = get_summary(SAP$NTL_RESULT),
  "NAP" = get_summary(NAP$NTL_RESULT),
  "WMT" = get_summary(WMT$NTL_RESULT),
  "NPL" = get_summary(NPL$NTL_RESULT),
  "XER" = get_summary(XER$NTL_RESULT),
  .id = "Region")

#create excel sheet of the eco-regions and summary stats 
wb <- createWorkbook()
addWorksheet(wb, "CHLA")
writeData(wb, "CHLA", CHLA_summary)
addWorksheet(wb, "PTL")
writeData(wb, "PTL", PTL_summary)
addWorksheet(wb, "NTL")
writeData(wb, "NTL", NTL_summary)
saveWorkbook(wb, "Nutrient_Summaries.xlsx", overwrite = TRUE)

#Overlap between sites ----
df1 <- HNLChlorophyll_sites_list %>%
  mutate(source = "HNL")

df2 <- byECO3_HNLChlorophyll_sites %>%
  mutate(source = "ECO3")

df3 <- byECO9_HNLChlorophyll_sites %>%
  mutate(source = "ECO9")

all_sites <- bind_rows(df1, df2, df3)

site_overlap <- all_sites %>%
  group_by(SITE_ID) %>%
  summarise(
    sources = paste(unique(source), collapse = ", "),
    n_sources = n_distinct(source),
    overlap_type = case_when(
      n_sources == 3 ~ "All three",
      n_sources == 2 ~ "Two datasets",
      n_sources == 1 ~ "Unique",
      TRUE ~ "Other"
    ), .groups = "drop")

#all data tossed into one sheet and what dataframe pulled from 
final_data <- all_sites %>% left_join(site_overlap, by = "SITE_ID")
site_overlap %>% count(overlap_type)
site_overlap <- all_sites %>%
  group_by(SITE_ID) %>%
  summarise(
    sources = paste(sort(unique(source)), collapse = " + "),
    n_sources = n_distinct(source),
    .groups = "drop")

final_unique <- final_data %>%
  group_by(SITE_ID) %>%
  summarise(
    LAT_DD83 = first(LAT_DD83),
    LON_DD83 = first(LON_DD83),
    PTL_RESULT = mean(PTL_RESULT, na.rm = TRUE),
    NTL_RESULT = mean(NTL_RESULT, na.rm = TRUE),
    CHLA_RESULT = mean(CHLA_RESULT, na.rm = TRUE),
    nutrient_group = first(nutrient_group),
    sources = first(sources),
    n_sources = first(n_sources),
    overlap_type = first(overlap_type),
    .groups = "drop")

#mapping by overlap ----
final_unique <- final_unique %>%
  mutate(overlap_label = case_when(
    n_sources == 1 ~ "Unique",
    n_sources == 2 ~ "Two datasets",
    n_sources == 3 ~ "All three"))

ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = final_unique,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = nutrient_group,
                 shape = factor(overlap_label)),
             size = 1.5,alpha = 0.8) +
  coord_fixed(1.3) +
  labs(
    title = "HNLC Chlorophyll Sites: Nutrient Groups and Dataset Overlap",
    color = "Nutrient Group",
    shape = "Overlap (# of datasets)"
  ) + theme_minimal() + theme(legend.position = "right")


#working on showing how many sites its in 
final_unique <- final_unique %>%
  mutate(source_combo = case_when(
    sources == "HNL" ~ "HNL only",
    sources == "ECO3" ~ "ECO3 only",
    sources == "ECO9" ~ "ECO9 only",
    sources %in% c("HNL, ECO3", "ECO3, HNL") ~ "HNL + ECO3",
    sources %in% c("HNL, ECO9", "ECO9, HNL") ~ "HNL + ECO9",
    sources %in% c("ECO3, ECO9", "ECO9, ECO3") ~ "ECO3 + ECO9",
    sources %in% c("HNL, ECO3, ECO9", "HNL, ECO9, ECO3", 
                   "ECO3, HNL, ECO9", "ECO3, ECO9, HNL",
                   "ECO9, HNL, ECO3", "ECO9, ECO3, HNL") ~ "All three",
    TRUE ~ "Other"))



#plot with each eco location 
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = final_unique,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = nutrient_group,
                 shape = source_combo),
             size = 1.5,
             alpha = 0.95) +
  coord_fixed(1.3) +
  labs(title = "HNLC Chlorophyll Sites: Nutrient Groups and Dataset Overlap",
    color = "Nutrient Group",
    shape = "Dataset Source Combination") +
  scale_shape_manual(values = c(
    "HNL only" = 16,
    "ECO3 only" = 17,
    "ECO9 only" = 15,
    "HNL + ECO3" = 3,
    "HNL + ECO9" = 7,
    "ECO3 + ECO9" = 8,
    "All three" = 18)) +
  theme_minimal() +
  theme(legend.position = "right")



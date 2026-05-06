library(dplyr)
library(tidyr)
library(dplyr)
library(tidyr)


#2018 2019 data set for emtals 

metals_long <- bind_rows(
  Iron_2018_2019 %>%
    mutate(
      Metal = "Iron",
      HUCEightDigitCode = as.character(HUCEightDigitCode)
    ),
  
  Manganese_2018_2019 %>%
    mutate(
      Metal = "Manganese",
      HUCEightDigitCode = as.character(HUCEightDigitCode)
    ),
  
  Nickel_2018_2019 %>%
    mutate(
      Metal = "Nickel",
      HUCEightDigitCode = as.character(HUCEightDigitCode)
    ),
  
  Zinc_2018_2019 %>%
    mutate(
      Metal = "Zinc",
      HUCEightDigitCode = as.character(HUCEightDigitCode)
    )
) %>%
  mutate(
    DL_ugL = DetectionQuantitationLimitMeasure.MeasureValue.Clean.UGL
  )




metals_wide <- metals_long %>%
  group_by(MonitoringLocationIdentifier, ActivityStartDate, Metal) %>%
  summarise(
    DL_ugL = mean(DL_ugL, na.rm = TRUE),
    HUCEightDigitCode = first(HUCEightDigitCode),
    lat.Clean = first(lat.Clean),
    lon.Clean = first(lon.Clean),
    Method = first(ResultAnalyticalMethod.MethodName),
    .groups = "drop",
    COMID = first(COMID)
  ) %>%
  pivot_wider(
    id_cols = c(
      MonitoringLocationIdentifier,
      ActivityStartDate,
      HUCEightDigitCode,
      lat.Clean,
      lon.Clean,
      Method,
      COMID
    ),
    names_from = Metal,
    values_from = DL_ugL
  )


#how many huc 8 our there 
library(dplyr)

n_unique <- n_distinct(HNLC_data$HUC8)
n_unique

n_total <- nrow(HNLC_data)
n_duplicates <- n_total - n_unique

n_total
n_duplicates

HNLC_data %>%
  count(HUC8, name = "n") %>%
  arrange(desc(n))

HNLC_data %>%
  count(HUC8) %>%
  summarise(
    appear_once = sum(n == 1),
    repeated = sum(n > 1)
  )

#frequencny of frequencys 
HNLC_data %>%
  count(HUC8) %>%          # count occurrences per HUC8
  count(n, name = "num_HUC8") %>%   # count how many HUC8s have that frequency
  arrange(n)

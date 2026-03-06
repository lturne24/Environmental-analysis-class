#mappping 
SiteInfo <- SiteInfo %>%
  mutate(VISIT_NO = as.integer(VISIT_NO))

combined_spatial <- full_data %>%
  left_join(
    SiteInfo %>%
      select(SITE_ID, LAT_DD83, LON_DD83, STATE_NM, US_L3NAME),
    by = "SITE_ID"
  )

combined_spatial <- full_data %>%
  left_join(
    SiteInfo %>%
      select(SITE_ID, VISIT_NO, LAT_DD83, LON_DD83, STATE_NM, US_L3NAME),
    by = c("SITE_ID", "VISIT_NO")
  )

table(combined_spatial$STATE_NM[combined_spatial$HNLC_stat == 1])

table(combined_spatial$STATE_NM[combined_spatial$HNLC_strict == 1])

#Map of all the High N High P Low chlorophyll sites 
us_states <- map_data("state")
ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = combined_spatial,
             aes(x = LON_DD83, y = LAT_DD83),
             color = "grey80",
             size = 1) +
  geom_point(data = subset(combined_spatial, HNLC_strict == 1),
             aes(x = LON_DD83, y = LAT_DD83),
             color = "blue",
             size = 2) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "High N + High P – Low Chlorophyll Sites",
       x = "Longitude",
       y = "Latitude")

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

#high N low c sites 
full_data <- full_data %>%
  mutate(
    HNLC_N_only = ifelse(
      NTL_RESULT > N_high &
        CHL < Chl_low,
      1, 0
    )
  )
SiteInfo <- SiteInfo %>%
  mutate(VISIT_NO = as.integer(VISIT_NO))

combined_spatial <- full_data %>%
  left_join(
    SiteInfo %>%
      select(SITE_ID, VISIT_NO, LAT_DD83, LON_DD83),
    by = c("SITE_ID", "VISIT_NO")
  )

#fucking big ass map


us_states <- map_data("state")

base_map <- ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  coord_fixed(1.3) +
  theme_minimal()
#high p and N
map_strict <- base_map +
  geom_point(data = combined_spatial,
             aes(x = LON_DD83, y = LAT_DD83),
             color = "grey85",
             size = 0.8) +
  geom_point(data = subset(combined_spatial, HNLC_strict == 1),
             aes(x = LON_DD83, y = LAT_DD83),
             color = "blue",
             size = 1.8) +
  labs(title = "High N + High P + Low Chlorophyll")
#High P
map_P <- base_map +
  geom_point(data = combined_spatial,
             aes(x = LON_DD83, y = LAT_DD83),
             color = "grey85",
             size = 0.8) +
  geom_point(data = subset(combined_spatial, HNLC_stat == 1),
             aes(x = LON_DD83, y = LAT_DD83),
             color = "red",
             size = 1.8) +
  labs(title = "High P + Low Chlorophyll")
#high N map 
map_N <- base_map +
  geom_point(data = combined_spatial,
             aes(x = LON_DD83, y = LAT_DD83),
             color = "grey85",
             size = 0.8) +
  geom_point(data = subset(combined_spatial, HNLC_N_only == 1),
             aes(x = LON_DD83, y = LAT_DD83),
             color = "green",
             size = 1.8) +
  labs(title = "High N + Low Chlorophyll")

map_strict + map_P + map_N

map_strict
map_P
map_N

#hiogh nurtient locations 
full_data <- full_data %>%
mutate(
  nutrient_group = case_when(
    PTL_RESULT > P_high & NTL_RESULT > N_high ~ "High N + High P",
    PTL_RESULT > P_high & NTL_RESULT <= N_high ~ "High P only",
    NTL_RESULT > N_high & PTL_RESULT <= P_high ~ "High N only",
    TRUE ~ "Not High Nutrients"
  )
)

SiteInfo <- SiteInfo %>%
  mutate(VISIT_NO = as.integer(VISIT_NO))

combined_spatial <- full_data %>%
  left_join(
    SiteInfo %>%
      select(SITE_ID, VISIT_NO, LAT_DD83, LON_DD83),
    by = c("SITE_ID", "VISIT_NO")
  )


us_states <- map_data("state")

ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.2) +
  geom_point(data = combined_spatial,
             aes(x = LON_DD83,
                 y = LAT_DD83,
                 color = nutrient_group),
             size = 1.8,
             alpha = 0.8) +
  coord_fixed(1.3) +
  scale_color_manual(values = c(
    "High N + High P" = "purple",
    "High P only" = "red",
    "High N only" = "green",
    "Not High Nutrients" = "grey85"
  )) +
  theme_minimal() +
  labs(title = "Spatial Distribution of High Nutrient Sites",
       x = "Longitude",
       y = "Latitude",
       color = "Nutrient Category")


# Map only low chlorophyll sites colored by nutrient group ----

full_data <- full_data %>%
  mutate(
    nutrient_group_lowCHL = case_when(
      PTL_RESULT > P_high & NTL_RESULT > N_high & CHL < Chl_low ~ "High N + High P, Low Chl",
      PTL_RESULT > P_high & NTL_RESULT <= N_high & CHL < Chl_low ~ "High P only, Low Chl",
      NTL_RESULT > N_high & PTL_RESULT <= P_high & CHL < Chl_low ~ "High N only, Low Chl",
      TRUE ~ NA_character_  # drop sites that don't meet low CHL criteria
    )
  ) %>%
  filter(!is.na(nutrient_group_lowCHL))


#map HNLC 
map_data_lowCHL <- full_data %>%
  filter(!is.na(nutrient_group_lowCHL)) %>%
  left_join(
    SiteInfo %>% select(SITE_ID, LAT_DD83, LON_DD83),
    by = "SITE_ID"
  ) %>%
  distinct(SITE_ID, LAT_DD83, LON_DD83, nutrient_group_lowCHL)

# Map
ggplot() +
  # Add US state outlines
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "black"
  ) +
  # Add site points
  geom_point(
    data = map_data_lowCHL,
    aes(x = LON_DD83, y = LAT_DD83, color = nutrient_group_lowCHL),
    size = 3,
    alpha = 0.7
  ) +
  # Colors
  scale_color_manual(
    values = c(
      "High N + High P, Low Chl" = "red",
      "High P only, Low Chl" = "blue",
      "High N only, Low Chl" = "green"
    )
  ) +
  coord_fixed(1.3) +  # roughly correct aspect ratio for US
  theme_minimal() +
  labs(
    color = "Nutrient Category",
    title = "Low Chlorophyll Sites by Nutrient Category",
    x = "Longitude",
    y = "Latitude"
  )

#map of sites----
full_data <- full_data %>%
  mutate(
    nutrient_lowCHL_group = case_when(
      PTL_RESULT > P_high & NTL_RESULT > N_high & CHL < Chl_low ~ "High N + High P + Low Chl",
      PTL_RESULT > P_high & NTL_RESULT <= N_high & CHL < Chl_low ~ "High P only + Low Chl",
      NTL_RESULT > N_high & PTL_RESULT <= P_high & CHL < Chl_low ~ "High N only + Low Chl",
      TRUE ~ "Background"
    )
  )

SiteInfo <- SiteInfo %>%
  mutate(VISIT_NO = as.integer(VISIT_NO))

combined_spatial <- full_data %>%
  left_join(
    SiteInfo %>%
      select(SITE_ID, VISIT_NO, LAT_DD83, LON_DD83, STATE_NM, US_L3NAME),
    by = c("SITE_ID", "VISIT_NO")
  )

us_states <- map_data("state")
ggplot() +
  # US outline
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "black",
    linewidth = 0.2
  ) +
  # Background sites
  geom_point(
    data = subset(combined_spatial,
                  nutrient_lowCHL_group == "Background"),
    aes(x = LON_DD83, y = LAT_DD83),
    color = "grey80",
    size = 1,
    alpha = 0.6
  ) +
  # Highlighted sites
  geom_point(
    data = subset(combined_spatial,
                  nutrient_lowCHL_group != "Background"),
    aes(x = LON_DD83,
        y = LAT_DD83,
        color = nutrient_lowCHL_group),
    size = 2.5,
    alpha = 0.9
  ) +
  scale_color_manual(values = c(
    "High N + High P + Low Chl" = "purple",
    "High P only + Low Chl" = "red",
    "High N only + Low Chl" = "green"
  )) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    title = "Low Chlorophyll Sites by Nutrient Category",
    x = "Longitude",
    y = "Latitude",
    color = "Category"
  )


#list of sites ----
high_nutrient_low_chl_sites <- combined_spatial %>%
  filter(nutrient_lowCHL_group != "Background") %>%
  distinct(
    SITE_ID,
    STATE_NM,
    US_L3NAME,
    LAT_DD83,
    LON_DD83,
    nutrient_lowCHL_group
  )

high_nutrient_low_chl_sites

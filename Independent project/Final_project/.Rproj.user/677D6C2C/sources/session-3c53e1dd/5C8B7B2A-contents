#exploring the water chem data 
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

cor(full_data$PTL_RESULT, full_data$CHL, use = "complete.obs") #.04
cor(full_data$NTL_RESULT, full_data$CHL, use = "complete.obs") #.07 Both hella weak 
#looks at correlation 
#if weak then strengthens HNLC rationale 


#scatterplots 
#total phosurpos v. chlorphyly 
ggplot(full_data, aes(x = PTL_RESULT, y = CHL)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

#total nitrogen v. chlorophyl 
ggplot(full_data, aes(x = NTL_RESULT, y = CHL)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

#correlation on log values 
log_cor_P <- cor(log10(full_data$PTL_RESULT[full_data$PTL_RESULT > 0]),
                 log10(full_data$CHL[full_data$PTL_RESULT > 0 & full_data$CHL > 0]),
                 use = "complete.obs")

log_cor_N <- cor(log10(full_data$NTL_RESULT[full_data$NTL_RESULT > 0]),
                 log10(full_data$CHL[full_data$NTL_RESULT > 0 & full_data$CHL > 0]),
                 use = "complete.obs")
log_cor_P
log_cor_N
#number of sites with top 25% high P 25%low Chlor
P_high <- quantile(full_data$PTL_RESULT, 0.75, na.rm = TRUE)
N_high <- quantile(full_data$NTL_RESULT, 0.75, na.rm = TRUE)
Chl_low <- quantile(full_data$CHL, 0.25, na.rm = TRUE)
full_data <- full_data %>%
  mutate(
    HNLC_stat = ifelse(
      PTL_RESULT > P_high &
        CHL < Chl_low,
      1, 0
    )
  )

table(full_data$HNLC_stat)
#7percent of site fall in Hp Lc

#number of sites with top 25% high N 25%low Chlor
full_data <- full_data %>%
  mutate(
    HNLC_strict = ifelse(
      PTL_RESULT > P_high &
        NTL_RESULT > N_high &
        CHL < Chl_low,
      1, 0
    )
  )

table(full_data$HNLC_strict)
#4pecent of sites Hn Lc

#all high P sites 30% have low c 
mean(
  full_data$CHL[full_data$PTL_RESULT > P_high] < Chl_low,
  na.rm = TRUE
)

#28percent  of Hn and P have low c
mean(
  full_data$CHL[
    full_data$PTL_RESULT > P_high &
      full_data$NTL_RESULT > N_high
  ] < Chl_low,
  na.rm = TRUE
)




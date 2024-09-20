#Load required packages
pacman::p_load(
  xgboost, 
  SHAPforxgboost, 
  data.table, 
  caret, 
  vip, 
  sf, 
  tidyverse, 
  table1, 
  ggcorrplot, 
  gridExtra, 
  leaflet, 
  randomForest, 
  glmnet, 
  plotly, 
  shiny, 
  cowplot
)

#Load nigdata as Rds extract from survey files
nigdata <- readRDS("nigdata.Rds")

# Education
education <- nigdata$sect2_education %>%
  group_by(hhid) %>%
  mutate(
    s02q05 = ifelse(any(s02q05 == 1), 1, 0),
    hle = ifelse(all(is.na(s02q07)), NA_real_, max(s02q07, na.rm = TRUE))
  ) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid, s02q05, hle) %>%
  rename(school = s02q05) %>%
  mutate(
    hle = case_when(
      is.na(hle) ~ NA_real_,
      hle > 20 ~ 1,
      TRUE ~ 0
    )
  )

#Health
health <- nigdata$sect3_health %>%
  group_by(hhid) %>%
  mutate(s03q03=ifelse(any(s03q03 == 1),1,0),
         s03q28=ifelse(any(s03q28 == 1),1,0),
         s03q19=ifelse(any(s03q19 == 1),1,0)) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid, s03q03, s03q19, s03q28) %>%
  rename(hospital_1m = s03q03, hosp_adm = s03q19, disability = s03q28)

#Labour

labour <- nigdata$sect4a1_labour %>%
  group_by(hhid) %>%
  mutate(
    s04aq16 = ifelse(any(s04aq16 == 1), 1, 0),
    s04aq06 = ifelse(any(s04aq06 == 1), 1, 0),
    s04aq50 = ifelse(any(s04aq50 == 1), 1, 0)
  ) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid, s04aq06, s04aq16, s04aq50) %>%
  rename(
    agrishort = s04aq06,
    job7d = s04aq16,
    agrilong = s04aq50
  )

# Loan
credit <- nigdata$sect11a_credit %>%
  select(hhid, s11q01, sector) %>%
  rename(loan = s11q01, urban=sector) %>%
  mutate(loan = ifelse(loan == 1, 1, 0),
         urban = ifelse(urban== 1, 1, 0))

#mean age
mean_age <- nigdata$sect1_roster %>%
  group_by(hhid) %>%
  summarize(meanage = mean(s01q04a)) %>%
  ungroup() %>%
  select(hhid, meanage)


# hhsize <- nigdata$totcons  %>%
#   select(hhid, hhsize)
hhsize <- nigdata$totcons %>%
  mutate(hhsize = ifelse(hhsize >= 5, 1, 0)) %>%
  select(hhid, hhsize)
#housing
housing <- nigdata$sect14_housing %>%
  select(hhid, s14q03, s14q40, s14q44, s14q45, s14q27, s14q32) %>%
  rename(
    home_ownership = s14q03,
    toilet = s14q40,
    toilet_share = s14q44,
    toilet_share_num = s14q45,
    drinking_water_rainy = s14q27,
    drinking_water_dry = s14q32
  ) %>%
  mutate(
    toilet = case_when(
      is.na(toilet) ~ NA_integer_,               
      toilet %in% c(1, 2, 3, 4, 5, 6, 7, 9) ~ 1,          
      toilet %in% c(8, 10, 11, 12,13) ~ 0,     
      TRUE ~ toilet                                  
    ),
    home_ownership = case_when(
      is.na(home_ownership) ~ NA_integer_,       
      home_ownership == 1 ~ 1,                   
      home_ownership %in% c(2,3,4) ~ 0,           
      TRUE ~ home_ownership                                  
    ),
    drinking_water_rainy = case_when(
      is.na(drinking_water_rainy) ~ NA_integer_,
      drinking_water_rainy %in% c(1, 2, 3, 4, 5, 6, 8, 10) ~ 1,
      drinking_water_rainy %in% c(7, 9, 11, 12, 13,14,15, 16, 17) ~ 0,
      TRUE ~ drinking_water_rainy
    ),
    drinking_water_dry = case_when(
      is.na(drinking_water_dry) ~ NA_integer_,  
      drinking_water_dry %in% c(1, 2, 3, 4, 5, 6, 8, 10) ~ 1,       
      drinking_water_dry %in% c(7, 9, 11, 12, 13,14,15, 16, 17) ~ 0,   
      TRUE ~ drinking_water_dry                                 
    )
  )

#crime
# crime <- nigdata$sect17_crime_security %>%
#   group_by(hhid) %>%
#   mutate(s17q01=ifelse(any(s17q01 == 1),1,0)) %>%
#   distinct(hhid, .keep_all = TRUE) %>%
#   select(hhid, s17q01) %>%
#   rename(crime = s17q01)

# #food security
# food_security <- nigdata$sect8_food_security %>%
#   group_by(hhid) %>%
#   mutate(s08q01=ifelse(any(s08q01 == 1),1,0)) %>%
#   distinct(hhid, .keep_all = TRUE) %>%
#   select(hhid, s08q01) %>%
#   rename(food_insecure = s08q01)

#asset ownership

dat <- nigdata$sect10_assets

assets <- dat[, c("hhid", "asset_cd", "s10q01")]

assets <- assets %>%
  pivot_wider(names_from = asset_cd, values_from = s10q01) %>%
  select(hhid, "312","322","317", "318", "327", "328", "319", "3322") %>%
  rename(refrigerator = "312",radio="322", bicycle="317",telephone="3322", motorbike="318", television = "327", computer = "328", vehicle = "319") %>%
  mutate(across(c(refrigerator, television, computer, vehicle, radio, bicycle, motorbike, telephone),
                ~ ifelse(. == 1, 1, 0)))



# food consumption
dat <- nigdata$sect6c_aggregate_food_1

cons <- dat[, c("hhid", "item_cd", "s06cq08")]

food_cons <- cons %>%
  pivot_wider(names_from = item_cd, values_from = s06cq08) %>%
  select(-"6") %>%
  rename(
    grains_flours = "1",
    starchy_roots = "2",
    pulses_nuts = "3",
    vegetables = "4",
    meat_fish = "5",
    fruits = "7",
    milk_products = "8",
    oils_fats = "9",
    sugar = "10",
    spices = "11"
  ) 
nga <- read.csv("nga.csv")


ngazn <- nga %>%
  select(hhid, zn_mg, folate_mcg) %>%
  mutate(zn_mg = ifelse(zn_mg < 10.2, 1, 0),
         folate_mcg = ifelse(folate_mcg < 250, 1, 0))

#dflist <- list(education, health, labour, credit, housing, assests, food_cons,hhsize, mean_age, ngazn) 
dflist <- list(education, health, labour, credit,assets, housing,food_cons,hhsize, mean_age, ngazn) 

# Merge all dataframes on 'hhid'
nig_df <- reduce(dflist, function(x, y) full_join(x, y, by = "hhid"))

#saveRDS(nig_df, file = "nig_df.Rds")

na_counts <- colSums(is.na(nig_df))


# Convert the result to a data frame for better readability
na_counts_df <- data.frame(Column = names(na_counts), NA_Count = na_counts)
na_counts_df <- na_counts_df[-1]
print(na_counts_df)
# Add a new column for missing percentage
d <- 2
na_counts_df$Missing_Percentage <- round((na_counts_df$NA_Count / 22126) * 100,d)
print(na_counts_df)



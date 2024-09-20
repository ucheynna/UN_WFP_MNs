tanz2020data <- readRDS("tanz2020data.Rds")
  
#Education
education <- tanz2020data$hh_sec_c %>%
  group_by(y5_hhid) %>%
  mutate(
    hh_c03 = ifelse(any(hh_c03 ==1), 1, 0),
    hle = ifelse(all(is.na(hh_c07)), NA_real_, max(hh_c07, na.rm = TRUE))
  )%>%
  distinct(y5_hhid, .keep_all = TRUE) %>%
  select(y5_hhid,hh_c03, hle) %>%
  rename(school = hh_c03) %>%
  mutate(
    hle = case_when(
      is.na(hle) ~ NA_real_,
      hle > 20 ~ 1,
      TRUE ~ 0
    )
  )

#Health
health <- tanz2020data$hh_sec_d %>%
  group_by(y5_hhid) %>%
  mutate(hh_d02 = ifelse(any(hh_d02 == 1), 1, 0),
         hh_d10 = ifelse(any(hh_d10 == 1), 1, 0),
         hh_d17 = ifelse(any(hh_d17 == 1), 1, 0),
         hh_d19 = ifelse(any(hh_d19 == 1), 1, 0),
         hh_d21 = ifelse(any(hh_d21 == 1), 1, 0),
         hh_d23 = ifelse(any(hh_d23 == 1), 1, 0),
         hh_d25 = ifelse(any(hh_d25 == 1), 1, 0),
         hh_d27 = ifelse(any(hh_d27 == 1), 1, 0)) %>%
  distinct(y5_hhid, .keep_all = TRUE) %>%
  rename(hospital_1m = hh_d02, hosp_adm = hh_d10) %>%
  mutate(disability = ifelse(hh_d17 != 1 | hh_d19 != 1 | hh_d21 != 1 | hh_d23 != 1 | hh_d25 != 1 | hh_d27 != 1, 1, 0)) %>%
  select(y5_hhid, hospital_1m, hosp_adm, disability) 

#labour
labour <- tanz2020data$hh_sec_e1 %>%
  group_by(y5_hhid) %>%
  mutate(hh_e03=ifelse(any(hh_e03 == 1),1,0),
         hh_e07=ifelse(any(hh_e07 == 1),1,0)) %>%
  distinct(y5_hhid, .keep_all = TRUE) %>%
  select(y5_hhid,hh_e07,hh_e03) %>%
  rename(agrishort =hh_e07,job7d = hh_e03)

#region
urban <- tanz2020data$hh_sec_a %>%
  mutate(urban = ifelse(y5_rural == 2, 1, 0)) %>%
  select(y5_hhid,y4_hhid, urban)

#credit
credit <- tanz2020data$hh_sec_p %>%
  select(y5_hhid, hh_p01) %>%
  rename(loan = hh_p01) %>%
  mutate(loan = ifelse(loan == 1, 1, 0))


hh_char <- tanz2020data$hh_sec_b %>%
  group_by(y5_hhid) %>%
  summarize(
    meanage = mean(hh_b04, na.rm = TRUE),
    hhsize = n_distinct(indidy5),
    agrilong = if_else(any(hh_b11 == 1), 1, 0)
  ) %>%
  ungroup() %>%
  mutate(hhsize = if_else(hhsize >= 5, 1, 0)) %>%
  select(y5_hhid, meanage, hhsize, agrilong)

#housing
housing <- tanz2020data$hh_sec_i %>%
  select(y5_hhid, hh_i01, hh_i12, hh_i13, hh_i14, hh_i19, hh_i29) %>%
  rename(
    home_ownership = hh_i01,        
    toilet = hh_i12,                
    toilet_share = hh_i13,          
    toilet_share_num = hh_i14,      
    drinking_water_rainy = hh_i19,  
    drinking_water_dry = hh_i29     
  ) %>%
  mutate(
    toilet = case_when(
      is.na(toilet) ~ NA_integer_,               
      toilet %in% c(3, 4, 5, 6, 7, 8) ~ 1,          
      toilet %in% c(1, 2, 9) ~ 0,     
      TRUE ~ toilet                               
    ),
    home_ownership = case_when(
      is.na(home_ownership) ~ NA_integer_,       
      home_ownership == 1 ~ 1,                   
      home_ownership %in% c(2,3,4,5,6) ~ 0,           
      TRUE ~ home_ownership                              
    ),
    drinking_water_rainy = case_when(
      is.na(drinking_water_rainy) ~ NA_integer_,                               
      drinking_water_rainy %in% c(1, 2, 3, 5,7) ~ 1,        
      drinking_water_rainy %in% c(4, 6, 8 ,9, 10 ,11 ,12) ~ 0,
      TRUE ~ drinking_water_rainy                    
    ),
    drinking_water_dry = case_when(
      is.na(drinking_water_dry) ~ NA_integer_,  
      drinking_water_dry %in% c(1, 2, 3, 5,7) ~ 1,       
      drinking_water_dry %in% c(4, 6, 8 ,9, 10 ,11 ,12) ~ 0,   
      TRUE ~ drinking_water_dry                        
    )
  )
#assets
dat <- tanz2020data$hh_sec_m

assets  <- dat[, c("y5_hhid", "itemcode", "hh_m00")]
# assests <- assests %>%
#   pivot_wider(names_from = itemcode, values_from = hh_m00)%>%
#   select(y5_hhid, "404", "406","415", "425") %>%
#   rename(refrigerator = "404", television = "406", computer = "415", vehicle = "425") %>%
#   mutate(across(c(refrigerator, television, computer, vehicle), 
#                 ~ ifelse(. == 2, 0, 1)))
assets <- assets %>%
  pivot_wider(names_from = itemcode, values_from = hh_m00)%>%
  select(y5_hhid, "404", "406","415", "425", "426", "427", "401", "403") %>%
  rename(refrigerator = "404", television = "406", computer = "415", vehicle = "425", motorbike="426", bicycle="427", radio="401", telephone="403") %>%
  mutate(across(c(refrigerator, television, computer, vehicle, radio, bicycle, motorbike, telephone),
                ~ ifelse(. == 0, 0, 1)))

#food cons
dat <- tanz2020data$hh_sec_j3

cons  <- dat[, c("y5_hhid", "itemcode", "hh_j08")]
food_cons <- cons %>%
  pivot_wider(names_from = itemcode, values_from = hh_j08) 



food_cons <- food_cons %>%
  rename(
    grains_flours = "1",
    starchy_roots = "2",
    pulses_nuts = "3",
    vegetables = "4",
    meat_fish = "5",
    fruits = "6",
    milk_products = "7",
    oils_fats = "8",
    sugar = "9",
    spices = "10"
  ) 

dflist <- list(education, health, labour, credit, urban, housing, assets, food_cons,hh_char) 

# Merge all dataframes on 'hhid'
tanz2020_df <- reduce(dflist, function(x, y) full_join(x, y, by = "y5_hhid"))


tanz2020_df <- tanz2020_df %>%
  filter(!is.na(y4_hhid)) %>%
  group_by(y4_hhid) %>%
  slice(1) %>%
  ungroup()


replace_mode <- function(x) {
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))  
  x[is.na(x)] <- mode_val  # Replace NA values with mode
  return(x)
}

#Replace missing values in agrijob column with mode
tanz2020_df$hle <- replace_mode(tanz2020_df$hle)
tanz2020_df$agrishort <- replace_mode(tanz2020_df$agrishort)
tanz2020_df$computer <- replace_mode(tanz2020_df$computer)
tanz2020_df$refrigerator <- replace_mode(tanz2020_df$refrigerator)
tanz2020_df$television <- replace_mode(tanz2020_df$television)
tanz2020_df$vehicle <- replace_mode(tanz2020_df$vehicle)
tanz2020_df$job7d <- replace_mode(tanz2020_df$job7d)
tanz2020_df$school <- replace_mode(tanz2020_df$school)
tanz2020_df$disability <- replace_mode(tanz2020_df$disability)
tanz2020_df$loan <- replace_mode(tanz2020_df$loan)
#tanz2020_df$grains_flours <- replace_mode(tanz2020_df$grains_flours)
# tanz2020_df$starchy_roots <- replace_mode(tanz2020_df$starchy_roots)
tanz2020_df$hosp_adm <- replace_mode(tanz2020_df$hosp_adm)
tanz2020_df$hospital_1m <- replace_mode(tanz2020_df$hospital_1m)
# tanz2020_df$vegetables <- replace_mode(tanz2020_df$vegetables)
# tanz2020_df$meat_fish <- replace_mode(tanz2020_df$meat_fish)
# tanz2020_df$fruits <- replace_mode(tanz2020_df$fruits)
# tanz2020_df$milk_products <- replace_mode(tanz2020_df$milk_products)
# tanz2020_df$oils_fats <- replace_mode(tanz2020_df$oils_fats)
# tanz2020_df$sugar <- replace_mode(tanz2020_df$sugar)
# tanz2020_df$spices <- replace_mode(tanz2020_df$spices)
# tanz2020_df$pulses_nuts <- replace_mode(tanz2020_df$pulses_nuts)
tanz2020_df$telephone <- replace_mode(tanz2020_df$telephone)



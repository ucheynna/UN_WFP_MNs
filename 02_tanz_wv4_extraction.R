tanzdata <- readRDS("tanzdata.Rds")
#Education
education <- tanzdata$hh_sec_c %>%
  group_by(y4_hhid) %>%
  mutate(
    hh_c03 = ifelse(any(hh_c03 ==1), 1, 0),
    hle = ifelse(all(is.na(hh_c07)), NA_real_, max(hh_c07, na.rm = TRUE))
  )%>%
  distinct(y4_hhid, .keep_all = TRUE) %>%
  select(y4_hhid, hh_c03, hle) %>%
  rename(school = hh_c03) %>%
  mutate(
    hle = case_when(
      is.na(hle) ~ NA_real_,
      hle > 20 ~ 1,
      TRUE ~ 0
    )
  )

#Health
health <- tanzdata$hh_sec_d %>%
  group_by(y4_hhid) %>%
  mutate(hh_d02 = ifelse(any(hh_d02 == 1), 1, 0),
         hh_d10 = ifelse(any(hh_d10 == 1), 1, 0),
         hh_d17 = ifelse(any(hh_d17 == 1), 1, 0),
         hh_d19 = ifelse(any(hh_d19 == 1), 1, 0),
         hh_d21 = ifelse(any(hh_d21 == 1), 1, 0),
         hh_d23 = ifelse(any(hh_d23 == 1), 1, 0),
         hh_d25 = ifelse(any(hh_d25 == 1), 1, 0),
         hh_d27 = ifelse(any(hh_d27 == 1), 1, 0)) %>%
  distinct(y4_hhid, .keep_all = TRUE) %>%
  rename(hospital_1m = hh_d02, hosp_adm = hh_d10) %>%
  mutate(disability = ifelse(hh_d17 != 1 | hh_d19 != 1 | hh_d21 != 1 | hh_d23 != 1 | hh_d25 != 1 | hh_d27 != 1, 1, 0)) %>%
  select(y4_hhid, hospital_1m, hosp_adm, disability) 



#labour
labour <- tanzdata$hh_sec_e %>%
  group_by(y4_hhid) %>%
  mutate(hh_e09=ifelse(any(hh_e09 == 1),1,0),
         hh_e08e=ifelse(any(hh_e08e == 1),1,0)) %>%
  distinct(y4_hhid, .keep_all = TRUE) %>%
  select(y4_hhid,hh_e08e,hh_e09) %>%
  rename(agrishort =hh_e08e,job7d = hh_e09)

#region
urban <- tanzdata$hh_sec_a %>%
  mutate(urban = ifelse(clustertype == 2, 1, 0)) %>%
  select(y4_hhid, urban)

#credit
credit <- tanzdata$hh_sec_p %>%
  mutate(loan = ifelse(is.na(loancode), 0, 1)) %>%
  distinct(y4_hhid, .keep_all = TRUE) %>%
  select(y4_hhid, loan)

# hh_char <- tanzdata$hh_sec_b %>%
#   group_by(y4_hhid) %>%
#   summarize(
#     meanage = mean(hh_b04, na.rm = TRUE),
#     hhsize = n_distinct(indidy4),
#     agrilong = if_else(any(hh_b11 == 1), 1, 0)
#   ) %>%
#   ungroup() %>%
#   select(y4_hhid, meanage, hhsize, agrilong)


hh_char <- tanzdata$hh_sec_b %>%
  group_by(y4_hhid) %>%
  summarize(
    meanage = mean(hh_b04, na.rm = TRUE),
    hhsize = n_distinct(indidy4),
    agrilong = if_else(any(hh_b11 == 1), 1, 0)
  ) %>%
  ungroup() %>%
  mutate(hhsize = if_else(hhsize >= 5, 1, 0)) %>%  # Modify hhsize based on the condition
  select(y4_hhid, meanage, hhsize, agrilong)


#housing
housing <- tanzdata$hh_sec_i %>%
  select(y4_hhid, hh_i01, hh_i12, hh_i13, hh_i14, hh_i19, hh_i29) %>%
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
      drinking_water_rainy %in% c(1, 2, 3, 5, 7) ~ 1,        
      drinking_water_rainy %in% c(4, 6, 8 ,9, 10 ,11 ,12) ~ 0,
      TRUE ~ drinking_water_rainy                    
    ),
    drinking_water_dry = case_when(
      is.na(drinking_water_dry) ~ NA_integer_,  
      drinking_water_dry %in% c(1, 2, 3, 5, 7) ~ 1,       
      drinking_water_dry %in% c(4, 6, 8, 9, 10, 11,12) ~ 0,   
      TRUE ~ drinking_water_dry                        
    )
  )
#assets
dat <- tanzdata$hh_sec_m

assets  <- dat[, c("y4_hhid", "itemcode", "hh_m01")]
assets <- assets %>%
  pivot_wider(names_from = itemcode, values_from = hh_m01)%>%
  select(y4_hhid, "404", "406","415", "425", "426", "427", "401", "403") %>%
  rename(refrigerator = "404", television = "406", computer = "415", vehicle = "425", motorbike="426", bicycle="427", radio="401", telephone="403") %>%
  mutate(across(c(refrigerator, television, computer, vehicle, radio, bicycle, motorbike, telephone),
                ~ ifelse(. == 0, 0, 1)))

#food cons
dat <- tanzdata$hh_sec_j3

cons  <- dat[, c("y4_hhid", "itemcode", "hh_j08_3")]
food_cons <- cons %>%
  pivot_wider(names_from = itemcode, values_from = hh_j08_3) 



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
  )  %>%
  select(-"NA") 

dflist <- list(education, health, assets, labour, urban, credit, housing, food_cons,hh_char) 

# Merge all dataframes on 'hhid'
tanz_df <- reduce(dflist, function(x, y) full_join(x, y, by = "y4_hhid"))
#saveRDS(tanz_df, file="tanz_df.Rds")

# tanz4geoloc <- tanzdata$hh_sec_a %>%
#   select(y4_hhid, domain)
# saveRDS(tanz4geoloc, file  ="tanz4geoloc.Rds")

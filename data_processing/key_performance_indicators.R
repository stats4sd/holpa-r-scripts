library(tidyverse)
library(jsonlite)
library(httr)
library(data.table)

################################################################################
# IMPORT DATA FROM DATABASE
################################################################################ 

source("data_processing/get_db_connection.R")

performance_indicators <- data%>%select(id)

################################################################################
# CROP HEALTH (KPI 1)
################################################################################

#Household
performance_indicators <- performance_indicators%>%
  left_join(data%>%select(id, "kpi1_crop_health" = crop_loss_perc))

#Fieldwork

tmp <- fieldwork_sites%>%
  group_by(farm_id)%>%
  summarise_at(
    vars(apperance_description, growth, disease_incidence, insect_incidence, enemy_abundance,
         weeds, natural_vegetation, management),
    function(x) median(x, na.rm = TRUE)
  )%>%
  rowwise()%>%
  mutate(kpi1b_crop_health = median(c_across(appearance_description:management), na.rm = TRUE))
  
performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi1b_crop_health))

################################################################################
# ANIMAL HEALTH (KPI 2)
################################################################################

## livestock health
tmp <- data%>%
  mutate(kpi2_animal_health = case_when(
    disease_injury == 0 ~ 5,
    disease_injury == 1 ~ 3.66,
    disease_injury == 2 ~ 2.33,
    disease_injury == 3 ~ 1
  ))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi2_animal_health))

## fish health
tmp <- data%>%
  mutate(kpi2b_fish_health = case_when(
    fish_disease == 0 ~ 5,
    fish_disease == 1 ~ 3.66,
    fish_disease == 2 ~ 2.33,
    fish_disease == 3 ~ 1
  ))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi2b_fish_health))

################################################################################
# SOIL HELATH (KPI 3)
################################################################################

tmp <- data%>%
  mutate(
    fert_score = case_when(
      soil_fertility == 3 ~ 5,
      soil_fertility == 2 ~ 3.66,
      soil_fertility == 1 ~ 2.33,
      soil_fertility == 0 ~ 1
    ),
    erosion_score = case_when(
      soil_erosion == 2 ~ 1,
      soil_erosion == 1 ~ 3,
      soil_erosion == 0 ~ 5
    )
  )%>%
  rowwise()%>%
  mutate(kpi3_soil_health = median(c_across(c(fert_score, erosion_score)), na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi3_soil_health))

################################################################################
# NUTRIENT USE (KPI 4)
################################################################################

################################################################################
# BIODIVERSITY - DESCRIPTIVE (KPI 5)
################################################################################

tmp <- data%>%
  mutate_at(
    vars(pollinator_diversity, pest_diversity, pest_enemy_diversity,
         mammal_diversity, tree_cover, tree_diversity),
    function(x) case_when(
      x == "high" ~ 5,
      x == "medium" ~ 3.66,
      x == "low" ~ 2.33,
      x == "none" | x == "999" ~ 1
    )
  )%>%
  rowwise()%>%
  mutate(
    kpi5a_animal_diversity = median(c_across(c(pollinator_diversity, pest_diversity,
                                             pest_enemy_diversity, mammal_diversity)),
                                    na.rm = TRUE),
    kpi5b_tree_diversity = median(c_across(c(tree_cover, tree_diversity)),
                                  na.rm = TRUE)
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi5a_animal_diversity, kpi5b_tree_diversity))

################################################################################
# BIODIVERSITY - RICHNESS (KPI 6)
################################################################################

## CROP RICHNESS

# repeat_crop_production_details <- data.frame(
#   id = 1:300,
#   farm_id = rep(c(1:100),3),
#   primary_crop_name = sample(c("maize", "cotton", "wheat", "millet", "rice", "sorghum"), size = 300, replace = TRUE),
#   primary_crop_area_ha = sample(1:5, size = 300, replace = TRUE)
# )
# 
# data <- data.frame(
#   id = 1:100,
#   crop_count = sample(3:10, size = 100, replace = TRUE),
#   crop_area_ha = sample(1:8, size = 100, replace = TRUE)
# )

tmp1 <- data%>%
  select(id, crop_count, crop_area_ha)%>%
  mutate(
    crop_richness_min = min(crop_count, na.rm = TRUE),
    crop_richness_max = max(crop_count, na.rm = TRUE),
    crop_richness_median = median(crop_count, na.rm = TRUE)
  )%>%
  mutate(kpi6a_crop_richness_index = 
           (crop_count - crop_richness_min)/(crop_richness_max-crop_richness_min)*100)

tmp2 <- data%>%
  select(id, seed_type, exotic_local)%>%
  mutate(
    exotic_local = ifelse(exotic_local == 6 | exotice_local == 7, NA, exotic_local),
  )%>%
  rowwise()%>%
  mutate(kpi6b_variety_richness = median(c_across(seed_type:exotic_local), na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp1%>%select(id, kpi6a_crop_richness_index))%>%
  left_join(tmp2%>%select(id, kpi6b_variety_richness))


################################################################################
# BIODIVERSITY - LANDSCAPE COMPLEXITY (KPI 7)
################################################################################

tmp <- data%>%
  mutate_at(
    vars(bushland_diversity, fallow_land_diversity, hedgerows_diversity,
         grassland_diversity, forest_patches_diversity, wetlands_diversity,
         woodlots_diversity),
    function(x) case_when(
      x == "high" ~ 5,
      x == "medium" ~ 3.66,
      x == "low" ~ 2.33,
      x == "none" ~ 1
    )
  )%>%
  rowwise()%>%
  mutate(
    lc1 = median(c_across(c(natural_vegetation, bushland, fallow_land, hedgerows,
                          grassland, ponds,forest_patches, wetlands, woodlots,
                          other_land_covering)),
                 na.rm = TRUE),
    lc2 = median(c_across(c(bushland_diversity, fallow_land_diversity,
                          hedgerows_diversity,grassland_diversity,
                          forest_patches_diversity, wetlands_diversity,
                          woodlots_diversity))),
    kpi7_landscape_complexity = median(c_across(lc1:lc2), na.rm = TRUE)
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi7_landscape_complexity))

################################################################################
# CLIMATE MITIGATION (KPI 8)
################################################################################

################################################################################
# WATER USE (KPI 9)
################################################################################

tmp <- data%>%
  mutate(
    months_without_stress = 12 - months_with_stress,
    kpi9_water_stress = (months_without_stress/12)*100
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi9_water_stress))


################################################################################
# ENERGY USE (KPI 10)
################################################################################

tmp <- data%>%
  mutate(
    renewable = ifelse(
      str_detect(irrigation_energy_types, 
                 "Animal_traction|Burning_plant_materials|Human_power|Solar_panel|Wind_turbine|Biogas|Cow_dung_cakes")|
        str_detect(tillage_energy_types, 
                   "Animal_traction|Burning_plant_materials|Human_power|Solar_panel|Wind_turbine|Biogas|Cow_dung_cakes")|
        str_detect(cooking_energy, 
                   "Animal_traction|Burning_plant_materials|Human_power|Solar_panel|Wind_turbine|Biogas|Cow_dung_cakes")|
        str_detect(food_energy_types, 
                   "Animal_traction|Burning_plant_materials|Human_power|Solar_panel|Wind_turbine|Biogas|Cow_dung_cakes")|
      1,
      0
    ),
    non_renewable = ifelse(
      str_detect(irrigation_energy_types, 
                 "Electricity|Gas|Coal|Petrol_or_diesel|LPG|Oil")|
        str_detect(tillage_energy_types, 
                   "Electricity|Gas|Coal|Petrol_or_diesel|LPG|Oil")|
        str_detect(cooking_energy, 
                   "Electricity|Gas|Coal|Petrol_or_diesel|LPG|Oil")|
        str_detect(food_energy_types, 
                   "Electricity|Gas|Coal|Petrol_or_diesel|LPG|Oil")|
        1,
      0
    )
  )%>%
  mutate(
    kpi10_energy_use = case_when(
      renewable == 1 & non_renewable == 0 ~ 5,
      renewable == 1 & non_renewable == 1 ~ 3,
      renewable == 0 & non_renewable == 1 ~ 1
    )
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi10_energy_use))

################################################################################
# INCOME (KPI 11)
################################################################################

#income sum - use reference values or if not available use mean income of the data (ADD OPTION FOR REFERENCE VALUES)

tmp <- data%>%
  #left_join(ref_values%>%filter(section == "income"))%>%
  group_by(team_id)%>%
  mutate(mean_income = mean(income_sum, na.rm = TRUE))%>%
  mutate(kpi11a_income_ratio = income_sum / coalesce(ref_value, mean_income))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi11a_income_ratio))

# Income stability
performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, "kpi11b_income_stability" = income_stability))

# Income vs expenditures
tmp <- data%>%
  mutate(farm_loss = na_if(farm_loss, 999))%>%
  mutate(farm_loss = 1-farm_loss)

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, "kpi11c_income_v_expenditures" = farm_loss))

# Income sufficiency
performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, "kpi11d_income_sufficiency" = sufficient_income))


################################################################################
# YIELD GAP (KPI 12)
################################################################################

################################################################################
# LABOUR PRODUCTIVITY (KPI 13)
################################################################################

################################################################################
# CLIMATE RESILIENCE (KPI 14)
################################################################################

## ABS Pillar

# data <- data.frame(
#   id = 1:100,
#   distanceunit_farmland = sample(c("km", "minutes"),size = 100, replace = TRUE),
#   transportation_farmland = sample(c("walking", "cycling", "motorbike", "car", "horse", "donkey"), size = 100, replace = TRUE),
#   distance_farmland = sample(0:25, size = 100, replace = TRUE),
#   distanceunit_freshwater = sample(c("km", "minutes"),size = 100, replace = TRUE),
#   transportation_freshwater = sample(c("walking", "cycling", "motorbike", "car", "horse", "donkey"), size = 100, replace = TRUE),
#   distance_freshwater = sample(0:25, size = 100, replace = TRUE),
#   piped_drinking_water = sample(0:1, size = 100, replace = TRUE), 
#   piped_toilet = sample(0:1, size = 100, replace = TRUE), 
#   electricity = sample(0:1, size = 100, replace = TRUE), 
#   waste_collection = sample(0:1, size = 100, replace = TRUE), 
#   phone_reception = sample(0:1, size = 100, replace = TRUE), 
#   internet = sample(0:1, size = 100, replace = TRUE)
# )

abs1 <- data%>%
  select(id, starts_with("distanceunit"), starts_with("transportation_"), starts_with("distance"))%>%
  pivot_longer(
    cols = distanceunit_farmland:distance_freshwater,
    names_to = c(".value", "place"),
    names_sep = "_"
  )%>%
  mutate(transportation = ifelse(distanceunit == "km", NA, transportation))%>%
  mutate(distance = ifelse(distance %in% c(99,999,9999,99999), NA, distance))%>%
  mutate(walking_time = case_when(
    distanceunit == "km" ~ (distance/5)*60,
    transportation %in% c("motorbike", "car") ~ distance*10,
    transportation == "cycling" ~ distance*3,
    transportation %in% c("horse", "donkey") ~ distance*2,
    transportation == "walking" ~ distance
  ))%>%
  mutate(
    walking_time_score = case_when(
      walking_time < 5 ~ 5,
      walking_time < 20 ~ 2.5,
      walking_time >= 20 ~ 0
    )
  )%>%
  group_by(id)%>%
  summarise(ABS_service_access = mean(walking_time_score, na.rm = TRUE))

abs2 <- data%>%
  select(id, piped_drinking_water, piped_toilet, electricity, waste_collection, phone_reception, internet)%>%
  rowwise()%>%
  mutate(ABS_utilities = sum(c_across(piped_drinking_water:internet), na.rm = TRUE))%>%
  mutate(ABS_utilities = case_when(
    ABS_utilities >= 3 ~ 5,
    ABS_utilities > 0 ~ 2.5,
    ABS_utilities == 0 ~ 0
  ))%>%
  select(id, ABS_utilities)

abs <- full_join(abs1, abs2)%>%
  rowwise()%>%
  mutate(ABS_score = mean(c_across(ABS_service_access:ABS_utilities), na.rm = TRUE))

## ASSET PILLAR

# data <- data.frame(
#   id = 1:100,
#   cars = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   motorbikes = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   bicycles = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   gas_cookers = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   electric_cookers = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   mobile_phones = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   smartphones = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   ox_plough = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   tractors = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   seed_drills = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   plows = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   crop_facilities = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1)),
#   asset_other_count = sample(0:5, size = 100, replace = TRUE, prob = c(10,1,1,1,1,1))
# )

ast <- data%>%
  select(id, cars:crop_facilities,asset_other_count)%>%
  mutate_at(vars(cars:asset_other_count), function(x) ifelse(x>0,1,0))%>%
  rowwise()%>%
  mutate(asset_sum = sum(c_across(cars:asset_other_count), na.rm = TRUE))%>%
  mutate(AST_score = case_when(
    asset_sum >= 3 ~ 5,
    asset_sum > 0 ~ 2.5,
    asset_sum == 0 ~ 0
  ))%>%
  select(id, AST_score)

## SOCIAL SECURITY PILLAR

# data <- data.frame(
#   id = 1:100,
#   free_school_meals = sample(1:5, size = 100, replace = TRUE),
#   support_banks = sample(c(0,1,999), size = 100, replace = TRUE),
#   support_community_leaders = sample(c(0,1,999), size = 100, replace = TRUE),
#   support_other_community = sample(c(0,1,999), size = 100, replace = TRUE),
#   support_my_community = sample(c(0,1,999), size = 100, replace = TRUE),
#   support_farmer_coops = sample(c(0,1,999), size = 100, replace = TRUE)
# )

ssn <- data%>%
  select(id, "SSN_school_meals" = free_school_meals, starts_with("support"))%>%
  mutate_at(vars(starts_with("support")), function(x) na_if(x, 999))%>%
  rowwise()%>%
  mutate(SSN_hh_support = sum(c_across(starts_with("support")), na.rm = TRUE))%>%
  mutate(SSN_hh_support = case_when(
    SSN_hh_support >= 3 ~ 5,
    SSN_hh_support > 0 ~ 2.5,
    SSN_hh_support == 0 ~ 0
  ))%>%
  mutate(SSN_score = mean(c_across(c(SSN_school_meals, SSN_hh_support)), na.rm = TRUE))

## ADAPTIVE CAPACITY PILLAR

ac <- data%>%
  select(id, read_write, highest_education_male, highest_education_female,
         agricultural_training, business_training, other_training_yn,
         income_count, food_expentiture_percent, credit_access,
         debt_repayment, debt, agricultural_loss_insurance, subsidies)%>%
  mutate(
    AC_literacy = case_when(
      read_write == 3 ~ 5,
      read_write %in% c(1,2) ~ 2.5,
      read_write %in% c(0,777) ~ 0
    ),
    highest_education_male = case_when(
      highest_education_male >= 2 ~ 5,
      highest_education_male == 1 ~ 2.5,
      highest_education_male == 0 ~ 0
    ),
    highest_education_female = case_when(
      highest_education_female >= 2 ~ 5,
      highest_education_female == 1 ~ 2.5,
      highest_education_female == 0 ~ 0
    ),
    AC_training = ifelse(
      agricultural_training == 1 |
        business_training == 1 |
          other_training_yn == 1, 5, 0
    ),
    AC_income_sources = case_when( #FLAG AS NOT CURRENRLY POSSIBLE TO EQUAL 0
      income_count >= 3 ~ 5,
      income_count > 0 ~ 2.5,
      income_count == 0 ~ 0
    ),
    AC_food_expenditure = case_when(
      food_expenditure_percent %in% ("0", "1_25") ~ 5 #CHECK ON 0,
      food_expenditure_percent == "26_50" ~ 3.33,
      food_expenditure_percent == "51_75" ~ 1.66,
      food_expenditure_percent == "76_100" ~ 0
    ),
    AC_credit_access = case_when(
      credit_access == 2 ~ 5,
      credit_access == 999 ~ NA,
      .default = 0
    ),
    AC_credit_repayment = case_when(
      credit_access == 2 & debt == 1 ~ 5,
      debt_repayment == 4 ~ 5,
      debt_repayment == 3 ~ 3.33,
      debt_repayment == 2 ~ 1.66,
      dept_repayment == 1 | credit_acess %in% c(1,0) ~ 0
    ),
    AC_insurance == case_when(
      agricultural_loss_insurance > 0 ~ 5,
      agricultural_loss_insurance == 0 ~ 0
    ),
    AC_subsidies = case_when(
      subsidies == 1 ~ 5,
      subsidies == 0 ~ 0
    )
  )%>%
  rowwise()%>%
  mutate(AC_education = mean(c_across(c(highest_education_male,
                                        highest_education_female)),
                             na.rm = TRUE))%>%
  mutate(AC_score = mean(c_across(starts_with("AC_")), na.rm = TRUE))

## RIMA

# ac <- data.frame(
#   id = 1:100,
#   AC_score = sample(0:5, size = 100, replace = TRUE)
# )

RIMA <- abs%>%
  select(id, ABS_score)%>%
  left_join(ast%>%select(id, AST_score))%>%
  left_join(ssn%>%select(id, SSN_score))%>%
  left_join(ac%>%select(id, AC_score))%>%
  rowwise()%>%
  mutate(kpi14a_climate_resilience = sum(c_across(ends_with("_score")), na.rm = TRUE))


performance_indicators <- performance_indicators%>%
  left_join(RIMA%>%select(id, kpi14a_climate_resilience))%>%
  left_join(data%>%select(id, "kpi_14b_climate_resilience" = capacity_to_recover))


################################################################################
# DIET QUALITY (KPI 15)
################################################################################

tmp <- data%>%
  mutate(
    grains = ifelse(baked == 1|grains == 1|tubers == 1,1,0),
    pulses = pulses,
    nuts_seeds = nuts,
    dairy = ifelse(cheese == 1|yogurt == 1|milk == 1),
    meats = ifelse(processed_mears==1|red_meat_ruminant==1|
                     red_meat_non_ruminant==1|poultry==1|seafood==1,1,0),
    eggs = eggs,
    dark_leafy_veg = darkgreen,
    vitA = ifelse(vitA_veg==1|vitA_fruit==1,1,0),
    veg = otherveg,
    fruit = ifelse(citrus==1|otherfruit==1)
  )%>%
  rowwise()%>%
  mutate(kpi15_diet_diversity = sum(c_across(c(grains,pulses,nuts_seeds,
                                             dairy, meats, eggs, 
                                             dark_leafy_veg,
                                             vitA, veg, fruit)), na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi15_diet_diversity))

################################################################################
# FARMER AGENCY (KPI 16)
################################################################################

tmp <- data%>%
  mutate(
    hhwomen_agency_step_now = na_if(hhwomen_agency_step_now,999),
    hhmen_agency_step_now = na_if(hhmen_agency_step_now,999)
  )%>%
  rowwise()%>%
  mutate(kpi16_farmer_agency = mean(c_across(c(hhwomen_agency_step_now,
                                             hhmen_agency_step_now)),
                                    na.rm = TRUE)
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi16_farmer_agency))

################################################################################
# LAND TENURE AND SECURITY (KPI 17)
################################################################################

tmp <- data%>%
  mutate(
    vulnerable_share = area_at_threat_ha/total_land_ha,
    land_owned_share = (area_owned_ha/total_land_ha)*100,
    kpi17a_land_security_perception = land_security_perception + (1-vulnerable_share),
    kpi17b_land_tenure = land_owned_share
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi17a_land_security_perception, kpi17b_land_tenure))

################################################################################
# HUMAN WELLBEING (KPI 18)
################################################################################

## MEAN OR MEDIAN _ ASK HOLPA??

tmp <- data%>%
  mutate_at(
    vars(wellbeing1:wellbeing12),
    function(x) na_if(x,999)
  )%>%
  rowwise()%>%
  mutate(kpi18_human_wellbeing = mean(c_across(wellbeing1:wellbeing12), na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi18_human_wellbeing))
  
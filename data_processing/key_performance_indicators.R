library(tidyverse)
library(jsonlite)
library(httr)
library(main_surveys.table)

################################################################################
# IMPORT main_surveys FROM DATABASE
################################################################################ 

source("data_processing//get_db_connection.R")

performance_indicators <- main_surveys%>%select(team_id, id)

################################################################################
# CROP HEALTH (KPI 1)
################################################################################

#Household
performance_indicators <- performance_indicators%>%
  left_join(main_surveys%>%
              select(id, "kpi1a_crop_health" = crop_loss_perc)%>%
              mutate(kpi1a_crop_health = 100 - kpi1a_crop_health)) # reverse to reflect percentage of crop retained (NOT lost)

#Fieldwork
tmp <- fieldwork_sites%>%
  mutate_at(
    vars(apperance_description, growth, disease_incidence, insect_incidence, enemy_abundance,
         weeds, natural_vegetation, management),
    function(x) na_if(x, 99)
  )%>%
  rowwise()%>%
  mutate(kpi1b_crop_health_fieldwork = median(c_across(appearance_description:management), na.rm = TRUE))%>%
  group_by(farm_id)%>%
  summarise(kpi1b_crop_health_fieldwork = median(kpi1b_crop_health_fieldwork, na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(farm_id, kpi1b_crop_health_fieldwork), by = c("id" = "farm_id"))

################################################################################
# ANIMAL HEALTH (KPI 2)
################################################################################

## livestock health
tmp <- main_surveys%>%
  mutate(kpi2_animal_health = case_when(
    disease_injury == 0 ~ 5,
    disease_injury == 1 ~ 3.66,
    disease_injury == 2 ~ 2.33,
    disease_injury == 3 ~ 1
  ))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi2a_animal_health))

## fish health
tmp <- main_surveys%>%
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

tmp <- main_surveys%>%
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

#first choice should be to use reference values here but could be difficult to effectively implement, 
# and most existing implementations do not have sufficient main_surveys

# nut_ref <- ref_fertiliser%>%
#   group_by(team_id)%>%
#   filter(!is.na(value))%>%
#   summarise(
#     n = n(),
#     ref_val = median(value)
#   )%>%
#   filter(n >= 10) # ASK HOLPA WHAT IS DEEMED TO BE SUFFICENT INFORMATION

tmp <- main_surveys%>%
  rowwise()%>%
  mutate(total_fertiliser_input = sum(c_across(chem_fert_kg_ha,
                                               own_organic_fert_kg_ha,
                                               bought_organic_fert_kg_ha), 
                                      na.rm = TRUE))%>%
  mutate(median_input = median(total_fertiliser_input, na.rm = TRUE))%>%
  #left_join(nut_ref, by = "team_id")%>%
  #mutate(ref_value_col = coalesce(ref_val, median_input))%>%
  #mutate(kpi4_nutrient_use = total_fertiliser_input/ref_value_col)%>%
  mutate(kpi4_nutrient_use = total_fertiliser_input/median_input)%>%
  mutate(kpi4_nutrient_use = ifelse(is.infinite(kpi4_nutrient_use) | 
                                      is.nan(kpi4_nutrient_use),NA,
                                    kpi4_nutrient_use
                                    ))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi4_nutrient_use))

################################################################################
# BIODIVERSITY - DESCRIPTIVE (KPI 5)
################################################################################

tmp <- main_surveys%>%
  mutate_at(
    vars(pollinator_diversity, pest_diversity, pest_enemy_diversity,
         mammal_diversity, tree_cover, tree_diversity),
    function(x) case_when(
      x == "high" ~ 5,
      x == "medium" ~ 3.66,
      x == "low" ~ 2.33,
      x == "none" ~ 1 
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

tmp1 <- main_surveys%>%
  select(id, crop_count, crop_area_ha)%>%
  mutate(
    crop_richness_min = min(crop_count, na.rm = TRUE),
    crop_richness_max = max(crop_count, na.rm = TRUE)
  )%>%
  mutate(kpi6a_crop_richness_index = 
           (crop_count - crop_richness_min)/(crop_richness_max-crop_richness_min)*100)

tmp2 <- main_surveys%>%
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

tmp <- main_surveys%>%
  mutate_at(
    vars(bushland_diversity, fallow_land_diversity, hedgerows_diversity,
         grassland_diversity, forest_patches_diversity, wetlands_diversity,
         woodlots_diversity),
    function(x) case_when(
      x == "high" ~ 5,
      x == "medium" ~ 3.66,
      x == "low" ~ 2.33,
      x == "none" ~ 1 #check as HOLPA script says NA rather than 1
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

tmp <- agroecological_practices%>%
  left_join(ref_cli_mitigation%>%
              select(practice_number, score_overall), by = "practice_number")%>%
  group_by(farm_id)%>%
  mutate(total_area = sum(practice_area_ha),
         practice_share = practice_area_ha/total_area,
         weighted_cc_score = score_overall * practice_share)%>%
  summarise(kpi8_climate_mitigation = mean(weighted_cc_score, na.rm = TRUE))
  
performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(farm_id, kpi8_climate_mitigation), by = c("id" = "farm_id"))

################################################################################
# WATER USE (KPI 9)
################################################################################

tmp <- main_surveys%>%
  mutate(
    months_without_stress = 12 - months_with_stress,
    kpi9_water_stress = (months_without_stress/12)*100
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi9_water_stress))


################################################################################
# ENERGY USE (KPI 10)
################################################################################

tmp <- main_surveys%>%
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

tmp <- main_surveys%>%
  left_join(ref_income%>%select(team_id, ref_income), by = "team_id")%>%
  group_by(team_id)%>%
  mutate(median_income = median(income_sum, na.rm = TRUE))%>% #HOLPA script used mean - median more suitable 
  mutate(kpi11a_income_ratio = income_sum / coalesce(ref_income, median_income))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi11a_income_ratio))

# Income stability
performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, "kpi11b_income_stability" = income_stability))

# Income vs expenditures
tmp <- main_surveys%>%
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

#based on medians

tmp <- crops%>%
  #left_join(ref_yield%>%select(team_id, crop_id, ref_yield), by = c("team_id", "crop_id"))%>%
  group_by(team_id, primary_crop_id)%>%
  mutate(median_yield_kg_ha = median(yield_kg_ha, na.rm = TRUE))%>%
  #mutate(ref_yield  = coalesce(ref_yield, median_yield_kg_ha))%>%
  ungroup()%>%
  #mutate(yield_ratio = yield_kg_ha/ref_yield)%>%
  mutate(yield_ratio = yield_kg_ha/median_yield_kg_ha)%>%
  mutate(yield_gap = ifelse(
    yield_ratio > 1, 0,
    ifelse(
      yield_ratio == 0, NA,
      (1 - yield_ratio)*100
    )
  ))%>%
  group_by(farm_id)%>%
  summarise(kpi12_yield_gap = mean(yield_gap, na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select("id" = farm_id, kpi12_yield_gap))

################################################################################
# LABOUR PRODUCTIVITY (KPI 13)
################################################################################

tmp_permanent <- permanent_labour%>%
  mutate(
    hh_hours_per_year = ifelse(labour_type == "household",
                               perm_labour_group_n_workers * perm_labour_hours * 365,0),
    hired_hours_per_year = ifelse(labour_type == "hired",
                                  perm_labourer_numbers * perm_labourer_hours * 365, 0)
  )%>%
  group_by(farm_id)%>%
  summarise(
    total_hh_hours_per_year = sum(hh_hours_per_year,na.rm = TRUE),
    total_hired_hours_per_year = sum(hired_hours_per_year, na.rm = TRUE)
  )
  
tmp_seasonal <- seasonal_labour%>%
  mutate(
    hh_hours_per_season = ifelse(labour_type == "household",
                                 seasonal_labour_n_working *
                                   seasonal_labour_hours *
                                   seasonal_labour_months_count * 30,0),
    hired_hours_per_season = ifelse(labour_type == "hired",
                                    seasonal_labourer_n_working *
                                      seasonal_labourer_hours *
                                      seasonal_labourer_months_count * 30,0)
  )%>%
  group_by(farm_id)%>%
  summarise(
    total_hh_seasonal_hours_per_year = sum(hh_hours_per_season, na.rm = TRUE),
    total_hired_seasonal_hours_per_year = sum(hired_hours_per_season, na.rm = TRUE)
  ) 

tmp_land <- main_surveys%>%
  select(id, total_crop_area_ha, livestock_land_own_ha, livestock_land_share_ha, fish_area_ha)%>%
  rowwise()%>%
  mutate(total_agricultural_land_ha = sum(c_across(total_crop_area_ha:fish_area_ha), 
                                          na.rm = TRUE))

tmp_income <- main_surveys%>%
  select(id, income_crops, income_livestock, income_fish)%>%
  rowwise()%>%
  mutate(total_agricultural_income = sum(c_across(income_crops:income_fish)))
  
tmp <- tmp_permanent%>%
  left_join(tmp_seasonal)%>%
  left_join(tmp_land, by = c("farm_id" = "id"))%>%
  left_join(tmp_income, by = c("farm_id" = "id"))%>%
  rowwise()%>%
  mutate(total_labour_hours_per_year = sum(c_across(total_hh_hours_per_year:total_hired_seasonal_hours_per_year),
                                             na.rm = TRUE))%>%
  mutate(kpi13a_labour_input = total_labour_hours_per_year/total_agricultural_land_ha)%>%
  mutate(kpi13b_labour_productivity = total_agricultural_income/total_labour_hours_per_year)

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi13a_labour_input,kpi13b_labour_productivity))
  
################################################################################
# CLIMATE RESILIENCE (KPI 14)
################################################################################

## ABS Pillar

abs1 <- main_surveys%>%
  select(id, starts_with("distanceunit"), starts_with("transportation_"), starts_with("distance"))%>%
  pivot_longer(
    cols = distanceunit_farmland:distance_freshwater,
    names_to = c(".value", "place"),
    names_sep = "_"
  )%>%
  mutate(transportation = ifelse(distanceunit == "km", NA, transportation))%>%
  mutate(distance = ifelse(distance %in% c(99,999,9999,99999), NA, distance))%>% #move to cleaning script
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

abs2 <- main_surveys%>%
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

ast <- main_surveys%>%
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

ssn <- main_surveys%>%
  select(id, "SSN_school_meals" = free_school_meals, starts_with("support_"))%>%
  mutate_at(vars(starts_with("support_")), function(x) na_if(x, 999))%>%
  rowwise()%>%
  mutate(SSN_hh_support = sum(c_across(starts_with("support_")), na.rm = TRUE))%>%
  mutate(SSN_hh_support = case_when(
    SSN_hh_support >= 3 ~ 5,
    SSN_hh_support > 0 ~ 2.5,
    SSN_hh_support == 0 ~ 0
  ))%>%
  mutate(SSN_score = mean(c_across(c(SSN_school_meals, SSN_hh_support)), na.rm = TRUE))

## ADAPTIVE CAPACITY PILLAR

ac <- main_surveys%>%
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
      food_expenditure_percent %in% ("0", "1_25") ~ 5
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

RIMA <- abs%>%
  select(id, ABS_score)%>%
  left_join(ast%>%select(id, AST_score))%>%
  left_join(ssn%>%select(id, SSN_score))%>%
  left_join(ac%>%select(id, AC_score))%>%
  rowwise()%>%
  mutate(kpi14a_climate_resilience = sum(c_across(ends_with("_score")), na.rm = TRUE))


performance_indicators <- performance_indicators%>%
  left_join(RIMA%>%select(id, kpi14a_climate_resilience))%>%
  left_join(main_surveys%>%select(id, "kpi_14b_climate_resilience" = capacity_to_recover))


################################################################################
# DIET QUALITY (KPI 15)
################################################################################

tmp <- main_surveys%>%
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

tmp <- main_surveys%>%
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

tmp <- main_surveys%>%
  mutate(
    vulnerable_share = ifelse(land_security_perception == 5, 0, area_at_threat_ha/total_land_ha),
    secure_share = 1 - vulnerable_share,
    land_owned_share = (area_owned_ha/total_land_ha)*100,
    kpi17a_land_security_perception = (land_security_perception * vulnerable_share) + (5 * secure_share),
    kpi17b_land_tenure = land_owned_share
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi17a_land_security_perception, kpi17b_land_tenure))

################################################################################
# HUMAN WELLBEING (KPI 18)
################################################################################

tmp <- main_surveys%>%
  mutate_at(
    vars(wellbeing1:wellbeing12),
    function(x) na_if(x,999)
  )%>%
  rowwise()%>%
  mutate(kpi18_human_wellbeing = median(c_across(wellbeing1:wellbeing12), na.rm = TRUE))

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi18_human_wellbeing))

################################################################################
# SCALING ALL INDICATORS TO 0 - 100
################################################################################

indicator_scale_set <- function(min, max, var){
  
  scaled_scores <- performance_indicators%>%
    select({{var}})%>%
    mutate({{var}} := case_when(
      {{var}} < min ~ min,
      {{var}} > max ~ max,
      .default = {{var}}))%>%
    mutate({{var}} := ({{var}} - min)/(max-min)*100)
  
  return(scaled_scores[[1]])
  
}

indicator_scale_main_surveys <- function(var){
  
  scaled_scores <- performance_indicators%>%
    group_by(team_id)%>%
    select(team_id, {{var}})%>%
    mutate(max_score = max({{var}}, na.rm = TRUE),
           min_score = min({{var}}, na.rm = TRUE))%>%
    mutate(
      score_scaled = ({{var}} - min_score)/(max_score-min_score)*100
    )
  
  return(scaled_scores$score_scaled)
  
}

indicator_scale_main_surveys_rev <- function(var){
  
  scaled_scores <- performance_indicators%>%
    group_by(team_id)%>%
    select(team_id, {{var}})%>%
    mutate(max_score = max({{var}}, na.rm = TRUE),
           min_score = min({{var}}, na.rm = TRUE))%>%
    mutate(
      score_scaled = 100 - (({{var}} - min_score)/(max_score-min_score)*100)
    )
  
  return(scaled_scores$score_scaled)
  
}

performance_indicators <- performance_indicators%>%
  mutate(
    kpi1a_crop_health_scaled = kpi1a_crop_health,
    kpi1b_crop_health_fieldwork_scaled = indicator_scale_set(1,5,kpi1b_crop_health_fieldwork),
    kpi2a_animal_health = indicator_scale_set(1,5,kpi2a_animal_health),
    kpi2b_fish_health_scaled = indicator_scale_set(1,5,kpi2b_fish_health),
    kpi3_soil_health_scaled = indicator_scale_set(1,5,kpi3_soil_health),
    kpi4_nutrient_use_scaled = indicator_scale_set(0.5,2,kpi4_nutrient_use),
    kpi5a_animal_diversity_scaled = indicator_scale_set(1,5,kpi5a_animal_diversity),
    kpi5b_tree_diversity_scaled = indicator_scale_set(1,5,kpi5b_tree_diversity),
    kpi6a_crop_richness_index_scaled = kpi6a_crop_richness_index,
    kpi6b_variety_richness_scaled = indicator_scale_set(1,5,kpi6b_variety_richness),
    kpi7_landscape_complexity_scaled = indicator_scale_set(1,5,kpi7_landscape_complexity),
    kpi8_climate_mitigation_scaled = indicator_scale_set(1,5,kpi8_climate_mitigation),
    kpi9_water_stress_scaled = kpi9_water_stress,
    kpi10_energy_use_scaled = indicator_scale_set(1,5,kpi10_energy_use),
    kpi11a_income_ratio_scaled = indicator_scale_set(0.5,2,kpi11a_income_ratio),
    kpi11b_income_stability_scaled = indicator_scale_set(1,5,kpi11b_income_stability),
    kpi11c_income_v_expenditures = indicator_scale_set(0,1,kpi11c_income_v_expenditures),
    kpi11d_income_sufficiency = indicator_scale_set(1,5,kpi11d_income_sufficiency),
    kpi12_yield_gap = indicator_scale_set(0,99,kpi12_yield_gap),
    kpi13a_labour_input_scaled = indicator_scale_main_surveys_rev(kpi13a_labour_input),
    kpi13b_labour_productivity_scaled  = indicator_scale_main_surveys(kpi13b_labour_productivity),
    kpi14a_climate_resilience_scaled = indicator_scale_set(0,20,kpi14a_climate_resilience),
    kpi14b_climate_resilience_scaled = indicator_scale_set(1,5,kpi14b_climate_resilience),
    kpi15_diet_diversity_scaled = indicator_scale_set(0,10,kpi15_diet_diversity),
    kpi16_farmer_agency_scaled = indicator_scale_set(1,5,kpi16_farmer_agency),
    kpi17a_land_security_perception_scaled = indicator_scale_set(1,5,kpi17a_land_security_perception),
    kpi17b_land_tenure = kpi17b_land_tenure,
    kpi18_human_wellbeing = indicator_scale_set(1,5,kpi18_human_wellbeing)
  )

################################################################################
# WRITE TABLE TO DATABASE
################################################################################

dbWriteTable(con,"performance_indicators",performance_indicators,overwrite=TRUE)

dbDisconnect(con)
  
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
  mutate(kpi3_soil_health = median(c_across(fert_score, erosion_score), na.rm = TRUE))

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
    kpi5a_animal_diversity = median(c_across(pollinator_diversity, pest_diversity,
                                             pest_enemy_diversity, mammal_diversity),
                                    na.rm = TRUE),
    kpi5b_tree_diversity = median(c_across(tree_cover, tree_diversity),
                                  na.rm = TRUE)
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi5a_animal_diversity, kpi5b_tree_diversity))

################################################################################
# BIODIVERSITY - RICHNESS (KPI 6)
################################################################################

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
    lc1 = median(c_across(natural_vegetation, bushland, fallow_land, hedgerows,
                          grassland, ponds,forest_patches, wetlands, woodlots,
                          other_land_covering),
                 na.rm = TRUE),
    lc2 = median(c_across(bushland_diversity, fallow_land_diversity,
                          hedgerows_diversity,grassland_diversity,
                          forest_patches_diversity, wetlands_diversity,
                          woodlots_diversity)),
    kpi7_landscape_complexity = median(c_across(lc1,lc2), na.rm = TRUE)
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
  mutate(kpi15_diet_diversity = sum(c_across(grains,pulses,nuts_seeds,
                                             dairy, meats, eggs, 
                                             dark_leafy_veg,
                                             vitA, veg, fruit), na.rm = TRUE))

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
  mutate(kpi16_farmer_agency = mean(c_across(hhwomen_agency_step_now,
                                             hhmen_agency_step_now),
                                    na.rm = TRUE)
  )

performance_indicators <- performance_indicators%>%
  left_join(tmp%>%select(id, kpi16_farmer_agency))

################################################################################
# LAND TENURE AND SECURITY (KPI 17)
################################################################################

tmp <- data%>%
  mutate(
    land_security_recalc = 
      case_when(
        land_security_perception == 5 ~ 0,
        land_security_perception == 4 ~ 0.25,
        land_security_perception == 3 ~ 0.5,
        land_security_perception == 2 ~ 0.75,
        land_security_perception == 1 ~ 1
      ),
    vulnerable_share = area_at_threat_ha/total_land_ha,
    land_owned_share = (area_owned_ha/total_land_ha)*100,
    land_security_weighted = 
     vulnerable_share * land_security_recalc,
    land_security_weighted = ifelse(
      land_security_perception == 5, 5,
      4 - (4*land_security_weighted)
  )
  )

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
  
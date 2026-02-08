library(tidyverse)
library(jsonlite)
library(httr)

################################################################################
# GENERATE KPI SCORES SHEET
# This script includes functions for each of the key performance indicators in HOLPA.
# Running the full script will build a new "performance_indicators" data frame. Each function adds to this data frame.

### REQUIREMENTS
# You should have a `main_surveys` data frame, plus the extra data frames listed in the README file.
# These are generated using either of the "get_data" scripts:
# - get_data_from_db.R
# - get_data_from_excel.R

################################################################################
# CROP HEALTH (KPI 1)
################################################################################

calculate_crop_health <- function(performance_indicators) {
  required_vars <- c("crop_loss_perc")

  main_survey <- ensure_required_vars_exist(required_vars, main_survey)

  # start snippet 2.1.a.crop_health_qualitative

  ## value is 100 - "% crop loss" as estimated by the farmer:
  performance_indicators <- performance_indicators %>%
    left_join(
      main_surveys %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          "kpi1a_crop_health" = crop_loss_perc
        ) %>%
        mutate(kpi1a_crop_health = 100 - kpi1a_crop_health)
    ) # reverse to reflect percentage of crop retained (NOT lost)
  # end snippet 2.1.a.crop_health_qualitative

  # start snippet 2.1.b.crop_health_quantitative
  tmp <- sites %>%
    ### confirm all variables are numeric and NAs if marked as '99'
    mutate_at(
      vars(
        appearance_description,
        growth,
        disease_incidence,
        insect_incidence,
        enemy_abundance,
        weeds,
        natural_vegetation,
        management
      ),
      function(x) as.numeric(x)
    ) %>%
    mutate_at(
      vars(
        appearance_description,
        growth,
        disease_incidence,
        insect_incidence,
        enemy_abundance,
        weeds,
        natural_vegetation,
        management
      ),
      function(x) na_if(x, 99)
    ) %>%
    rowwise() %>%

    ## Take the median of all the variables

    mutate(
      kpi1b_crop_health_fieldwork = median(
        c_across(appearance_description:management),
        na.rm = TRUE
      )
    ) %>%

    ## take the median of all 3 fields per farm

    group_by(farm_id, owner_id, submission_id) %>%
    summarise(
      kpi1b_crop_health_fieldwork = median(
        kpi1b_crop_health_fieldwork,
        na.rm = TRUE
      )
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(tmp %>% select(farm_id, owner_id, kpi1b_crop_health_fieldwork))
  # end snippet 2.1.b.crop_health_quantitative

  return(performance_indicators)
}


################################################################################
# ANIMAL HEALTH (KPI 2)
################################################################################

calculate_animal_health <- function(performance_indicators) {
  required_vars <- c("disease_injury", "fish_disease")

  main_survey <- ensure_required_vars_exist(required_vars, main_survey)

  ## start snippet 2.2.animal_health
  tmp <- main_surveys %>%

    ### map the 4 response options to a score from 1 - 5
    mutate(
      kpi2a_animal_health = case_when(
        disease_injury == 0 ~ 5,
        disease_injury == 1 ~ 3.66,
        disease_injury == 2 ~ 2.33,
        disease_injury == 3 ~ 1
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi2a_animal_health)
    )

  ## fish health
  tmp <- main_surveys %>%

    ### map the 4 response options to a score from 1 - 5
    mutate(
      kpi2b_fish_health = case_when(
        fish_disease == 0 ~ 5,
        fish_disease == 1 ~ 3.66,
        fish_disease == 2 ~ 2.33,
        fish_disease == 3 ~ 1
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi2b_fish_health)
    )

  # end snippet 2.2.animal_health

  return(performance_indicators)
}


################################################################################
# SOIL HELATH (KPI 3)
################################################################################

calculate_soil_health <- function(performance_indicators) {
  required_vars <- c("soil_fertility", "erosion")

  main_survey <- ensure_required_vars_exist(required_vars, main_survey)

  # start snippet 2.3.soil_health
  tmp <- main_surveys %>%

    ### Map options per question to a score from 1 - 5
    mutate(
      fert_score = case_when(
        soil_fertility == 3 ~ 5,
        soil_fertility == 2 ~ 3.66,
        soil_fertility == 1 ~ 2.33,
        soil_fertility == 0 ~ 1
      ),
      erosion_score = case_when(
        erosion == 2 ~ 1,
        erosion == 1 ~ 3,
        erosion == 0 ~ 5
      )
    ) %>%

    ## Take the median score of the questions
    rowwise() %>%
    mutate(
      kpi3_soil_health = median(
        c_across(c(fert_score, erosion_score)),
        na.rm = TRUE
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi3_soil_health)
    )

  # end snippet 2.3.soil_health

  return(performance_indicators)
}

################################################################################
# NUTRIENT USE (KPI 4)
################################################################################

calculate_nutrient_use <- function(performance_indicators) {
  required_vars <- c(
    "chem_fert_kg_ha",
    "own_organic_fert_kg_ha",
    "bought_organic_fert_kg_ha"
  )

  main_survey <- ensure_required_vars_exist(required_vars, main_survey)

  # start snippet 2.4.nutrient_use
  # first choice should be to use reference values here but could be difficult to effectively implement,
  # and most existing implementations do not have sufficient main_surveys

  ## So, by default, we reference the 'standard' values from the ref_crops data.

  nut_ref <- ref_crops %>%
    group_by(team_id) %>%
    filter(!is.na(recommended_fert_use)) %>%
    summarise(
      n = n(),
      ref_val = median(recommended_fert_use)
    )

  tmp <- main_surveys %>%
    rowwise() %>%

    ## sum all fertiliser input (chemical, own organic + bought organic)
    mutate(
      total_fertiliser_input = sum(
        c_across(c(
          chem_fert_kg_ha,
          own_organic_fert_kg_ha,
          bought_organic_fert_kg_ha
        )),
        na.rm = TRUE
      )
    ) %>%

    ## take the median overall input from all surveys for comparison
    mutate(median_input = median(total_fertiliser_input, na.rm = TRUE)) %>%
    left_join(nut_ref, by = c("owner_id" = "team_id")) %>%

    ## value = total / ref or total / median
    mutate(kpi4_nutrient_use_ref = total_fertiliser_input / ref_val) %>%
    mutate(kpi4_nutrient_use_median = total_fertiliser_input / median_input) %>%

    ## fix infinite or nan values to NA
    mutate(
      kpi4_nutrient_use_ref = ifelse(
        is.infinite(kpi4_nutrient_use_ref) |
          is.nan(kpi4_nutrient_use_ref),
        NA,
        kpi4_nutrient_use_ref
      )
    ) %>%
    mutate(
      kpi4_nutrient_use_median = ifelse(
        is.infinite(kpi4_nutrient_use_median) |
          is.nan(kpi4_nutrient_use_median),
        NA,
        kpi4_nutrient_use_median
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          kpi4_nutrient_use_ref,
          kpi4_nutrient_use_median
        )
    )

  # end snippet 2.4.nutrient_use

  return(performance_indicators)
}

################################################################################
# BIODIVERSITY - DESCRIPTIVE (KPI 5)
################################################################################

calculate_biodiversity_descriptive <- function(performance_indicators) {
  required_vars <- c(
    "pollinator_diversity",
    "pest_diversity",
    "pest_enemy_diversity",
    "mammal_diversity",
    "tree_cover",
    "tree_diversity"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate_at(
      vars(
        pollinator_diversity,
        pest_diversity,
        pest_enemy_diversity,
        mammal_diversity,
        tree_cover,
        tree_diversity
      ),
      function(x) {
        case_when(
          x == "high" ~ 5,
          x == "medium" ~ 3.66,
          x == "low" ~ 2.33,
          x == "none" ~ 1
        )
      }
    ) %>%
    rowwise() %>%
    mutate(
      kpi5a_animal_diversity = median(
        c_across(c(
          pollinator_diversity,
          pest_diversity,
          pest_enemy_diversity,
          mammal_diversity
        )),
        na.rm = TRUE
      ),
      kpi5b_tree_diversity = median(
        c_across(c(tree_cover, tree_diversity)),
        na.rm = TRUE
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          kpi5a_animal_diversity,
          kpi5b_tree_diversity
        )
    )

  return(performance_indicators)
}

################################################################################
# BIODIVERSITY - RICHNESS (KPI 6)
################################################################################

calculate_biodiversity_richness <- function(performance_indicators) {
  required_vars <- c("crops_count", "seed_type", "exotic_local")

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  ## CROP RICHNESS

  tmp1 <- main_surveys %>%
    select(farm_id, owner_id, submission_id, crops_count) %>%
    mutate(
      crop_richness_min = min(crops_count, na.rm = TRUE),
      crop_richness_max = max(crops_count, na.rm = TRUE)
    ) %>%
    mutate(
      kpi6a_crop_richness_index = (crops_count - crop_richness_min) /
        (crop_richness_max - crop_richness_min) *
        100
    )

  tmp2 <- main_surveys %>%
    select(farm_id, owner_id, submission_id, seed_type, exotic_local) %>%
    mutate_at(vars(seed_type:exotic_local), as.numeric) %>%
    mutate(
      exotic_local = ifelse(
        exotic_local == 6 | exotic_local == 7,
        NA,
        exotic_local
      ),
    ) %>%
    rowwise() %>%
    mutate(
      kpi6b_variety_richness = median(
        c_across(seed_type:exotic_local),
        na.rm = TRUE
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp1 %>%
        select(farm_id, owner_id, submission_id, kpi6a_crop_richness_index)
    ) %>%
    left_join(
      tmp2 %>% select(farm_id, owner_id, submission_id, kpi6b_variety_richness)
    )

  return(performance_indicators)
}


################################################################################
# BIODIVERSITY - LANDSCAPE COMPLEXITY (KPI 7)
################################################################################

calculate_biodiversity_landscape_complexity <- function(
  performance_indicators
) {
  required_vars <- c(
    "natural_vegetation",
    "bushland",
    "fallow_land",
    "hedgerows",
    "grassland",
    "ponds",
    "forest_patches",
    "wetlands",
    "woodlots",
    "other_land_covering",
    "bushland_diversity",
    "fallow_land_diversity",
    "hedgerows_diversity",
    "grassland_diversity",
    "forest_patches_diversity",
    "wetlands_diversity",
    "woodlots_diversity"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 1.3.landscape_complexity
  tmp <- main_surveys %>%

    ### ensure required variables are numeric
    mutate_at(
      vars(
        natural_vegetation,
        bushland,
        fallow_land,
        hedgerows,
        grassland,
        ponds,
        forest_patches,
        wetlands,
        woodlots,
        other_land_covering
      ),
      as.numeric
    ) %>%

    ### map diversity variables from survey optiosn to scale of 1 - 5
    mutate_at(
      vars(
        bushland_diversity,
        fallow_land_diversity,
        hedgerows_diversity,
        grassland_diversity,
        forest_patches_diversity,
        wetlands_diversity,
        woodlots_diversity
      ),
      function(x) {
        case_when(
          x == "high" ~ 5,
          x == "medium" ~ 3.66,
          x == "low" ~ 2.33,
          x == "none" ~ 1 #check as HOLPA script says NA rather than 1
        )
      }
    ) %>%
    rowwise() %>%
    mutate(
      ### take the median of each landscape type score
      lc1 = median(
        c_across(c(
          natural_vegetation,
          bushland,
          fallow_land,
          hedgerows,
          grassland,
          ponds,
          forest_patches,
          wetlands,
          woodlots,
          other_land_covering
        )),
        na.rm = TRUE
      ),

      ### take the median of the calculated diversity scores

      lc2 = median(
        c_across(c(
          bushland_diversity,
          fallow_land_diversity,
          hedgerows_diversity,
          grassland_diversity,
          forest_patches_diversity,
          wetlands_diversity,
          woodlots_diversity
        )),
        na.rm = TRUE
      ),

      ### overall landscape complexity is the median of the 2 medians

      kpi7_landscape_complexity = median(c_across(lc1:lc2), na.rm = TRUE)
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(farm_id, owner_id, submission_id, kpi7_landscape_complexity)
    )

  # end snippet 1.3.landscape_complexity

  return(performance_indicators)
}

################################################################################
# CLIMATE MITIGATION (KPI 8)
################################################################################

calculate_climate_mitigation <- function(performance_indicators) {
  required_vars <- c("practice_number", "practice_area_ha")

  for (i in required_vars) {
    if (i %!in% colnames(ecological_practices)) {
      ecological_practices <- ecological_practices %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 1.6.climate_mitigation

  ### join the practices listed in the survey data with the reference scores
  tmp <- ecological_practices %>%
    left_join(
      ref_cli_mitigation %>%
        select(ag_practice_id, score),
      by = c("practice_number" = "ag_practice_id")
    ) %>% #change to number when possible

    ## group by farm; calculate total area of each practice, the share of total area per practice and the weighted score
    ## (practice scores weighted by area of land that practice is implemented on)
    group_by(farm_id, owner_id, submission_id) %>%
    mutate(
      total_area = sum(practice_area_ha),
      practice_share = practice_area_ha / total_area,
      weighted_cc_score = score * practice_share
    ) %>%

    ## overall KPI is the mean of all weighted scores

    summarise(kpi8_climate_mitigation = mean(weighted_cc_score, na.rm = TRUE))

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi8_climate_mitigation)
    )

  # end snippet 1.6.climate_mitigation

  return(performance_indicators)
}

################################################################################
# WATER STRESS (KPI 9)
################################################################################

calculate_water_use <- function(performance_indicators) {
  required_vars <- c("months_with_stress")

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 1.4.water_stress

  ## invert `months_with_stress` and normalise to 100%.
  ## Higher scores -> less water stress.
  tmp <- main_surveys %>%
    mutate(
      months_without_stress = 12 - months_with_stress,
      kpi9_water_stress = (months_without_stress / 12) * 100
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi9_water_stress)
    )

  # end snippet 1.4.water_stress
}

################################################################################
# ENERGY USE (KPI 10)
################################################################################

calculate_energy_use <- function(performance_indicators) {
  required_vars <- c(
    "irrigation_energy_types",
    "tillage_energy_types",
    "cooking_energy_types",
    "food_energy_types"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 1.5.energy_use

  ### use string detection to check if "renewable" and/or "non-renewable" sources of energy are used on farm.
  tmp <- main_surveys %>%
    mutate(
      renewable = ifelse(
        str_detect(
          irrigation_energy_types,
          "animal_traction|burning_plant_materials|human_power|solar_panel|wind_turbine|biogas|cow_dung_cakes"
        ) |
          str_detect(
            tillage_energy_types,
            "animal_traction|burning_plant_materials|human_power|solar_panel|wind_turbine|biogas|cow_dung_cakes"
          ) |
          str_detect(
            cooking_energy_types,
            "animal_traction|burning_plant_materials|human_power|solar_panel|wind_turbine|biogas|cow_dung_cakes"
          ) |
          str_detect(
            food_energy_types,
            "animal_traction|burning_plant_materials|human_power|solar_panel|wind_turbine|biogas|cow_dung_cakes"
          ),
        1,
        0
      ),
      non_renewable = ifelse(
        str_detect(
          irrigation_energy_types,
          "electricity|gas|coal|petrol_or_diesel|lpg|oil"
        ) |
          str_detect(
            tillage_energy_types,
            "electricity|gas|coal|petrol_or_diesel|lpg|oil"
          ) |
          str_detect(
            cooking_energy_types,
            "electricity|gas|coal|petrol_or_diesel|lpg|oil"
          ) |
          str_detect(
            food_energy_types,
            "electricity|gas|coal|petrol_or_diesel|lpg|oil"
          ),
        1,
        0
      )
    ) %>%

    ## Only renewable = max score
    ## Mixture of both = mid score
    ## only non-renewable = low score

    mutate(
      kpi10_energy_use = case_when(
        renewable == 1 & non_renewable == 0 ~ 5,
        renewable == 1 & non_renewable == 1 ~ 3,
        renewable == 0 & non_renewable == 1 ~ 1
      )
    )

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi10_energy_use)
    )

  # end snippet 1.5.energy_use

  return(performance_indicators)
}
################################################################################
# TOTAL INCOME & INCOME STABILITY (KPI 11)
################################################################################

calculate_income <- function(performance_indicators) {
  required_vars <- c(
    "income_sum",
    "income_stability",
    "farm_loss",
    "sufficient_income"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 4.3.a.income_ratio

  ## get the reference income for the current country(ies)
  tmp_income_ref <- teams %>%
    select(id, country_id) %>%
    left_join(ref_income %>% select(-id))

  tmp <- main_surveys %>%
    left_join(tmp_income_ref %>% select(id, gni), by = c("owner_id" = "id")) %>%

    ## group by 'owner_id' (team_id) to get the overall median income from the full survey

    group_by(owner_id) %>%
    mutate(median_income = median(income_sum, na.rm = TRUE)) %>%

    ## calculate the 2 income ratios - one using the GNI from reference data, one from median_income calculated from this dataset
    mutate(kpi11a_income_ratio_ref = income_sum / gni) %>%
    mutate(kpi11a_income_ratio_median = income_sum / median_income)

  ### include the new variable in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          kpi11a_income_ratio_ref,
          kpi11a_income_ratio_median
        )
    )

  # end snippet 4.3.a.income_ratio

  # start snippet 4.3.b.income_stability

  ## income_stability is taken directly from the ODK survey data
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        mutate(income_stability = as.numeric(income_stability)) %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          "kpi11b_income_stability" = income_stability
        )
    )

  # end snippet 4.3.b.income_stability

  # start snippet 4.3.c.income_v_expenditures
  tmp <- main_surveys %>%

    ## ensure farm-loss is numeric and inverted
    mutate(farm_loss = as.numeric(farm_loss)) %>%
    mutate(farm_loss = na_if(farm_loss, 999)) %>%
    mutate(farm_loss = 1 - farm_loss)

  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          "kpi11c_income_v_expenditures" = farm_loss
        )
    )
  # end snippet 4.3.c.income_v_expenditures

  # start snippet 4.3.d.income_sufficiency

  ## Income sufficiency is taken directly from the ODK survey data
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%

        ## ensure it is numeric
        mutate(sufficient_income = as.numeric(sufficient_income)) %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          "kpi11d_income_sufficiency" = sufficient_income
        )
    ) #note typo in variable name

  # end snippet 4.3.d.income_sufficiency

  return(performance_indicators)
}

################################################################################
# YIELD GAP (KPI 12)
################################################################################

calculate_yield_gap <- function(performance_indicators) {
  required_vars <- c("primary_crop_id", "yield_kg")

  for (i in required_vars) {
    if (i %!in% colnames(crops)) {
      crops <- crops %>%
        mutate(!!i := NA)
    }
  }

  #based on medians
  tmp <- crops %>%
    left_join(
      ref_crops %>% select(team_id, name, expected_yield),
      by = c("owner_id" = "team_id", "primary_crop_id" = "name")
    ) %>%
    group_by(owner_id, primary_crop_id) %>%
    mutate(median_yield_kg_ha = median(as.numeric(yield_kg), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(yield_ratio_median = as.numeric(yield_kg) / median_yield_kg_ha) %>%
    mutate(
      yield_gap_median = ifelse(
        yield_ratio_median > 1,
        0,
        ifelse(
          yield_ratio_median == 0,
          NA,
          (1 - yield_ratio_median) * 100
        )
      )
    ) %>%
    mutate(yield_ratio_ref = as.numeric(yield_kg) / expected_yield) %>%
    mutate(
      yield_gap_ref = ifelse(
        yield_ratio_ref > 1,
        0,
        ifelse(
          yield_ratio_ref == 0,
          NA,
          (1 - yield_ratio_ref) * 100
        )
      )
    ) %>%
    group_by(farm_id, owner_id, submission_id) %>%
    summarise(
      kpi12_yield_gap_ref = mean(yield_gap_ref, na.rm = TRUE),
      kpi12_yield_gap_median = mean(yield_gap_median, na.rm = TRUE)
    )

  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          kpi12_yield_gap_ref,
          kpi12_yield_gap_median
        )
    )

  return(performance_indicators)
}
################################################################################
# LABOUR PRODUCTIVITY (KPI 13)
################################################################################

calculate_labour_productivity <- function(performance_indicators) {
  required_vars <- c("perm_labour_group_n_workers", "perm_labour_hours")

  for (i in required_vars) {
    if (i %!in% colnames(permanent_workers)) {
      permanent_workers <- permanent_workers %>%
        mutate(!!i := NA)
    }
  }

  # required_vars <- c("seasonal_labour_n_working", "seasonal_labour_hours", "seasonal_labour_months_count")
  #
  # for(i in required_vars){
  #
  #   if(i %!in% colnames(seasonal_workers)){
  #
  #     seasonal_workers <- seasonal_workers%>%
  #       mutate(!!i := NA)
  #
  #   }
  #
  # }

  required_vars <- c(
    "total_crop_area_ha",
    "livestock_land_own_ha",
    "livestock_land_share_ha",
    "fish_area_ha",
    "income_crops",
    "income_livestock",
    "income_fish"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 4.2.labour_productivity

  #NEEDS TO ACCOUNT FOR HIRED WOKERS EITHER BY COALESCING TABLE IN PROCESSING OR CALCUALTING HERE

  ## calculate hours_per_year for permanent and seasonal workers
  tmp_permanent <- permanent_workers %>%
    mutate(
      hours_per_year = perm_labour_group_n_workers * perm_labour_hours * 365
    ) %>%
    group_by(farm_id) %>%
    summarise(total_perm_hours_per_year = sum(hours_per_year, na.rm = TRUE))

  tmp_seasonal <- seasonal_workers %>%
    mutate(
      hours_per_season = seasonal_labour_n_working *
        seasonal_labour_hours *
        seasonal_labour_months_count *
        30
    ) %>%
    group_by(farm_id) %>%
    summarise(
      total_seasonal_hours_per_year = sum(hours_per_season, na.rm = TRUE)
    )

  ## calculate total agricultural land of all types
  tmp_land <- main_surveys %>%
    select(
      farm_id,
      total_crop_area_ha,
      livestock_land_own_ha,
      livestock_land_share_ha,
      fish_area_ha
    ) %>%
    rowwise() %>%
    mutate(
      total_agricultural_land_ha = sum(
        c_across(total_crop_area_ha:fish_area_ha),
        na.rm = TRUE
      )
    )

  ## calculate total agricultural income from all types
  tmp_income <- main_surveys %>%
    select(farm_id, income_crops, income_livestock, income_fish) %>%
    rowwise() %>%
    mutate(total_agricultural_income = sum(c_across(income_crops:income_fish)))

  ## bring calculated sets together
  tmp <- tmp_permanent %>%
    left_join(tmp_seasonal) %>%
    left_join(tmp_land) %>%
    left_join(tmp_income) %>%
    rowwise() %>%

    ## total labour hours per year = permanent + seasonal totals
    mutate(
      total_labour_hours_per_year = sum(
        c_across(total_perm_hours_per_year:total_seasonal_hours_per_year),
        na.rm = TRUE
      )
    ) %>%

    ## labour input = hours / land area
    mutate(
      kpi13a_labour_input = total_labour_hours_per_year /
        total_agricultural_land_ha
    ) %>%

    ## labour productivity = income / labour hours
    mutate(
      kpi13b_labour_productivity = total_agricultural_income /
        total_labour_hours_per_year
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, kpi13a_labour_input, kpi13b_labour_productivity)
    )

  # end snippet 4.2.labour_productivity

  return(performance_indicators)
}
################################################################################
# CLIMATE RESILIENCE (KPI 14)
################################################################################

calculate_climate_resilience <- function(performance_indicators) {
  required_vars <- c(
    "highest_education_male",
    "highest_education_female",
    "asset_other_count",
    "debt_repayment",
    "debt"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  ## start snippet 4.4.1.climate_resilience_abs

  ## Access to basic services (ABS)

  abs1 <- main_surveys %>%
    select(
      farm_id,
      owner_id,
      submission_id,
      starts_with("distanceunit"),
      starts_with("transportation_"),
      starts_with("distance")
    ) %>%
    pivot_longer(
      cols = distanceunit_farmland:distance_freshwater,
      names_to = c(".value", "place"),
      names_sep = "_"
    ) %>%
    mutate(
      transportation = ifelse(distanceunit == "km", NA, transportation)
    ) %>%
    mutate(
      distance = ifelse(distance %in% c(99, 999, 9999, 99999), NA, distance)
    ) %>% #move to cleaning script
    mutate(
      walking_time = case_when(
        distanceunit == "km" ~ (distance / 5) * 60,
        transportation %in% c("motorbike", "car") ~ distance * 10,
        transportation == "cycling" ~ distance * 3,
        transportation %in% c("horse", "donkey") ~ distance * 2,
        transportation == "walking" ~ distance
      )
    ) %>%
    mutate(
      walking_time_score = case_when(
        walking_time < 5 ~ 5,
        walking_time < 20 ~ 2.5,
        walking_time >= 20 ~ 0
      )
    ) %>%
    group_by(farm_id, owner_id, submission_id) %>%
    summarise(ABS_service_access = mean(walking_time_score, na.rm = TRUE))

  abs2 <- main_surveys %>%
    select(
      farm_id,
      owner_id,
      submission_id,
      piped_drinking_water,
      piped_toilet,
      electricity,
      waste_collection,
      phone_reception,
      internet
    ) %>%
    mutate_at(vars(piped_drinking_water:internet), as.numeric) %>%
    rowwise() %>%
    mutate(
      ABS_utilities = sum(c_across(piped_drinking_water:internet), na.rm = TRUE)
    ) %>%
    mutate(
      ABS_utilities = case_when(
        ABS_utilities >= 3 ~ 5,
        ABS_utilities > 0 ~ 2.5,
        ABS_utilities == 0 ~ 0
      )
    ) %>%
    select(farm_id, owner_id, submission_id, ABS_utilities)

  abs <- full_join(abs1, abs2) %>%
    rowwise() %>%
    mutate(
      ABS_score = mean(c_across(ABS_service_access:ABS_utilities), na.rm = TRUE)
    )

  ## end snippet 4.4.1.climate_resilience_abs
  ## start snippet 4.4.2.climate_resilience_assets

  ### Assets (AST)
  ast <- main_surveys %>%
    select(
      farm_id,
      owner_id,
      submission_id,
      cars:crop_facilities,
      asset_other_count
    ) %>%
    mutate_at(vars(cars:asset_other_count), as.numeric) %>%
    mutate_at(vars(cars:asset_other_count), function(x) ifelse(x > 0, 1, 0)) %>%
    rowwise() %>%
    mutate(asset_sum = sum(c_across(cars:asset_other_count), na.rm = TRUE)) %>%
    mutate(
      AST_score = case_when(
        asset_sum >= 3 ~ 5,
        asset_sum > 0 ~ 2.5,
        asset_sum == 0 ~ 0
      )
    ) %>%
    select(farm_id, owner_id, submission_id, AST_score)

  ## end snippet 4.4.2.climate_resilience_assets
  ## start snippet 4.4.3.climate_resilience_ssn

  ## SOCIAL Safety Nets (SSN)

  ssn <- main_surveys %>%
    select(
      farm_id,
      owner_id,
      submission_id,
      "SSN_school_meals" = free_school_meals,
      starts_with("support_")
    ) %>%
    mutate_at(vars(starts_with("support_")), as.numeric) %>%
    mutate_at(vars(starts_with("support_")), function(x) na_if(x, 999)) %>%
    mutate(SSN_school_meals = as.numeric(SSN_school_meals)) %>%
    rowwise() %>%
    mutate(
      SSN_hh_support = sum(c_across(starts_with("support_")), na.rm = TRUE)
    ) %>%
    mutate(
      SSN_hh_support = case_when(
        SSN_hh_support >= 3 ~ 5,
        SSN_hh_support > 0 ~ 2.5,
        SSN_hh_support == 0 ~ 0
      )
    ) %>%
    mutate(
      SSN_score = mean(
        c_across(c(SSN_school_meals, SSN_hh_support)),
        na.rm = TRUE
      )
    )

  ## end snippet 4.4.3.climate_resilience_ssn
  ## start snippet 4.4.4.climate_resilience_adaptive

  ## ADAPTIVE CAPACITY PILLAR

  ac <- main_surveys %>%
    select(
      farm_id,
      owner_id,
      submission_id,
      read_write,
      highest_education_male,
      highest_education_female,
      agricultural_training,
      business_training,
      other_training_yn,
      income_count,
      food_expenditure_percent,
      credit_access,
      debt_repayment,
      debt,
      agricultural_loss_insurance,
      subsidies
    ) %>%
    mutate_at(
      vars(
        read_write,
        highest_education_male,
        highest_education_female,
        agricultural_training,
        business_training,
        other_training_yn,
        income_count,
        credit_access,
        debt_repayment,
        debt,
        agricultural_loss_insurance,
        subsidies
      ),
      as.numeric
    ) %>%
    mutate(
      AC_literacy = case_when(
        read_write == 3 ~ 5,
        read_write %in% c(1, 2) ~ 2.5,
        read_write %in% c(0, 777) ~ 0
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
          other_training_yn == 1,
        5,
        0
      ),
      AC_income_sources = case_when(
        #FLAG AS NOT CURRENRLY POSSIBLE TO EQUAL 0
        income_count >= 3 ~ 5,
        income_count > 0 ~ 2.5,
        income_count == 0 ~ 0
      ),
      AC_food_expenditure = case_when(
        food_expenditure_percent %in% c("0", "1_25") ~ 5,
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
        debt_repayment == 1 | credit_access %in% c(1, 0) ~ 0
      ),
      AC_insurance = case_when(
        agricultural_loss_insurance > 0 ~ 5,
        agricultural_loss_insurance == 0 ~ 0
      ),
      AC_subsidies = case_when(
        subsidies == 1 ~ 5,
        subsidies == 0 ~ 0
      )
    ) %>%
    rowwise() %>%
    mutate(
      AC_education = mean(
        c_across(c(highest_education_male, highest_education_female)),
        na.rm = TRUE
      )
    ) %>%
    mutate(AC_score = mean(c_across(starts_with("AC_")), na.rm = TRUE))

  ## end snippet 4.4.4.climate_resilience_adaptive
  ## start snippet 4.4.climate_resilience_rima

  ## Calculate the overall Resilience Index Measurement and Analysis score

  RIMA <- abs %>%
    select(farm_id, owner_id, submission_id, ABS_score) %>%
    left_join(ast %>% select(farm_id, owner_id, submission_id, AST_score)) %>%
    left_join(ssn %>% select(farm_id, owner_id, submission_id, SSN_score)) %>%
    left_join(ac %>% select(farm_id, owner_id, submission_id, AC_score)) %>%
    rowwise() %>%
    mutate(
      kpi14a_climate_resilience = sum(
        c_across(ends_with("_score")),
        na.rm = TRUE
      )
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      RIMA %>%
        select(farm_id, owner_id, submission_id, kpi14a_climate_resilience)
    ) %>%
    left_join(
      main_surveys %>%
        mutate(kpi14b_climate_resilience = as.numeric(capacity_to_recover)) %>%
        select(farm_id, owner_id, submission_id, kpi14b_climate_resilience)
    )

  ## end snippet 4.4.climate_resilience_rima

  return(performance_indicators)
}

################################################################################
# DIET QUALITY (KPI 15)
################################################################################

calculate_diet_quality <- function(performance_indicators) {
  # start snippet 3.1.diet_quality

  tmp <- main_surveys %>%

    ## calculate main food group presence from sub-groups
    mutate(
      grains = ifelse(baked == 1 | grains == 1 | tubers == 1, 1, 0),
      pulses = as.numeric(pulses),
      nuts_seeds = as.numeric(nuts),
      dairy = ifelse(cheese == 1 | yogurt == 1 | milk == 1, 1, 0),
      meats = ifelse(
        processed_meats == 1 |
          red_meat_ruminant == 1 |
          red_meat_non_ruminant == 1 |
          poultry == 1 |
          seafood == 1,
        1,
        0
      ),
      eggs = as.numeric(eggs),
      dark_leafy_veg = as.numeric(darkgreen),
      vitA = ifelse(vita_veg == 1 | vita_fruit == 1, 1, 0),
      veg = as.numeric(otherveg),
      fruit = ifelse(citrus == 1 | otherfruit == 1, 1, 0)
    ) %>%
    rowwise() %>%
    mutate(
      ## indicator is sum of main food groups

      kpi15_diet_diversity = sum(
        c_across(c(
          grains,
          pulses,
          nuts_seeds,
          dairy,
          meats,
          eggs,
          dark_leafy_veg,
          vitA,
          veg,
          fruit
        )),
        na.rm = TRUE
      )
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi15_diet_diversity)
    )

  # end snippet 3.1.diet_quality

  return(performance_indicators)
}
################################################################################
# FARMER AGENCY (KPI 16)
################################################################################

calculate_farmer_agency <- function(performance_indicators) {
  required_vars <- c("hhwomen_agency_step_now", "hhmen_agency_step_now")

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 3.3.farmer_agency

  tmp <- main_surveys %>%

    ## check values are numeric and make na if given 'na' code
    mutate(
      hhwomen_agency_step_now = as.numeric(hhwomen_agency_step_now),
      hhmen_agency_step_now = as.numeric(hhmen_agency_step_now)
    ) %>%
    mutate(
      hhwomen_agency_step_now = na_if(hhwomen_agency_step_now, 999),
      hhmen_agency_step_now = na_if(hhmen_agency_step_now, 999)
    ) %>%
    rowwise() %>%

    ## overall farmer agency is mean of hhwomen and hhmen agency scores
    mutate(
      kpi16_farmer_agency = mean(
        c_across(c(hhwomen_agency_step_now, hhmen_agency_step_now)),
        na.rm = TRUE
      )
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi16_farmer_agency)
    )

  # end snippet 3.3.farmer_agency

  return(performance_indicators)
}

################################################################################
# LAND TENURE AND SECURITY (KPI 17)
################################################################################

calculate_land_tenure <- function(performance_indicators) {
  required_vars <- c(
    "land_security_perception",
    "area_at_threat_ha",
    "total_land_ha",
    "area_owned_ha"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  # start snippet 3.2.land_tenure

  tmp <- main_surveys %>%
    mutate(
      # vulnerable_share of land is 0 if perception of security is max.
      # otherwise it's the % of total_land under threat
      vulnerable_share = ifelse(
        land_security_perception == 5,
        0,
        area_at_threat_ha / total_land_ha
      ),

      ## invert for the `secure_share`
      secure_share = 1 - vulnerable_share,

      ## land owned share = % of total land that is owned by the farm household
      land_owned_share = (area_owned_ha / total_land_ha) * 100,

      ## calculate overall land_security_perception KPI
      kpi17a_land_security_perception = (land_security_perception *
        vulnerable_share) +
        (5 * secure_share),
      kpi17b_land_tenure = land_owned_share
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>%
        select(
          farm_id,
          owner_id,
          submission_id,
          kpi17a_land_security_perception,
          kpi17b_land_tenure
        )
    )

  # end snippet 3.2.land_tenure

  return(performance_indicators)
}
################################################################################
# HUMAN WELLBEING (KPI 18)
################################################################################

calculate_human_wellbeing <- function(performance_indicators) {
  # start snippet 3.4.human_well_being
  tmp <- main_surveys %>%
    ## check values are numeric and coded as na if 'na' code is used
    mutate_at(
      vars(wellbeing1:wellbeing12),
      as.numeric
    ) %>%
    mutate_at(
      vars(wellbeing1:wellbeing12),
      function(x) na_if(x, 999)
    ) %>%
    rowwise() %>%
    mutate(
      ## overall wellbeing is median of individual scores

      kpi18_human_wellbeing = median(
        c_across(wellbeing1:wellbeing12),
        na.rm = TRUE
      )
    )

  ### include the new variables in the performance_indicators data frame
  performance_indicators <- performance_indicators %>%
    left_join(
      tmp %>% select(farm_id, owner_id, submission_id, kpi18_human_wellbeing)
    )

  # end snippet 3.4.human_well_being

  return(performance_indicators)
}
################################################################################
# SCALING ALL INDICATORS TO 0 - 100
################################################################################

# -----------------------
# SCALING FUNCTIONS
# -----------------------
indicator_scale_set <- function(min, max, var) {
  scaled_scores <- performance_indicators %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} < min ~ min,
        {{ var }} > max ~ max,
        .default = {{ var }}
      )
    ) %>%
    mutate({{ var }} := ({{ var }} - min) / (max - min) * 100)

  return(scaled_scores[[1]])
}

indicator_scale_main_surveys <- function(var) {
  scaled_scores <- performance_indicators %>%
    group_by(owner_id) %>%
    select(owner_id, {{ var }}) %>%
    mutate(
      max_score = max({{ var }}, na.rm = TRUE),
      min_score = min({{ var }}, na.rm = TRUE)
    ) %>%
    mutate(
      score_scaled = ({{ var }} - min_score) / (max_score - min_score) * 100
    )

  return(scaled_scores$score_scaled)
}

indicator_scale_main_surveys_rev <- function(var) {
  scaled_scores <- performance_indicators %>%
    group_by(owner_id) %>%
    select(owner_id, {{ var }}) %>%
    mutate(
      max_score = max({{ var }}, na.rm = TRUE),
      min_score = min({{ var }}, na.rm = TRUE)
    ) %>%
    mutate(
      score_scaled = 100 -
        (({{ var }} - min_score) / (max_score - min_score) * 100)
    )

  return(scaled_scores$score_scaled)
}


############################################
# MAIN CALCULATE FUNCTION
############################################
calculate_key_performance_indicators <- function() {
  ## generate empty data frame for indicators
  performance_indicators <- main_surveys %>%
    select(farm_id, owner_id, submission_id)

  ## calculate each set of indicators into the data frame
  performance_indicators <- calculate_crop_health(performance_indicators)
  performance_indicators <- calculate_animal_health(performance_indicators)
  performance_indicators <- calculate_soil_health(performance_indicators)
  performance_indicators <- calculate_nutrient_use(performance_indicators)
  performance_indicators <- calculate_biodiversity_descriptive(
    performance_indicators
  )
  performance_indicators <- calculate_biodiversity_richness(
    performance_indicators
  )
  performance_indicators <- calculate_climate_mitigation(performance_indicators)
  performance_indicators <- calculate_water_use(performance_indicators)
  performance_indicators <- calculate_energy_use(performance_indicators)
  performance_indicators <- calculate_income(performance_indicators)
  performance_indicators <- calculate_yield_gap(performance_indicators)
  performance_indicators <- calculate_labour_productivity(
    performance_indicators
  )
  performance_indicators <- calculate_climate_resilience(performance_indicators)
  performance_indicators <- calculate_diet_quality(performance_indicators)
  performance_indicators <- calculate_farmer_agency(performance_indicators)
  performance_indicators <- calculate_land_tenure(performance_indicators)
  performance_indicators <- calculate_human_wellbeing(performance_indicators)

  ## Scale each set of indicators to 0-100 using scaling functions
  performance_indicators <- performance_indicators %>%
    mutate(
      kpi1a_crop_health_scaled = kpi1a_crop_health, #1
      kpi1b_crop_health_fieldwork_scaled = indicator_scale_set(
        1,
        5,
        kpi1b_crop_health_fieldwork
      ), #2
      kpi2a_animal_health = indicator_scale_set(1, 5, kpi2a_animal_health), #3
      kpi2b_fish_health_scaled = indicator_scale_set(1, 5, kpi2b_fish_health), #4
      kpi3_soil_health_scaled = indicator_scale_set(1, 5, kpi3_soil_health), #5
      kpi4_nutrient_use_ref_scaled = indicator_scale_set(
        0.5,
        2,
        kpi4_nutrient_use_ref
      ), #6
      kpi4_nutrient_use__median_scaled = indicator_scale_set(
        0.5,
        2,
        kpi4_nutrient_use_median
      ), #6
      kpi5a_animal_diversity_scaled = indicator_scale_set(
        1,
        5,
        kpi5a_animal_diversity
      ), #7
      kpi5b_tree_diversity_scaled = indicator_scale_set(
        1,
        5,
        kpi5b_tree_diversity
      ), #8
      kpi6a_crop_richness_index_scaled = kpi6a_crop_richness_index, #9
      kpi6b_variety_richness_scaled = indicator_scale_set(
        1,
        5,
        kpi6b_variety_richness
      ), #10
      kpi7_landscape_complexity_scaled = indicator_scale_set(
        1,
        5,
        kpi7_landscape_complexity
      ), #11
      kpi8_climate_mitigation_scaled = indicator_scale_set(
        1,
        5,
        kpi8_climate_mitigation
      ), #12
      kpi9_water_stress_scaled = kpi9_water_stress, #13
      kpi10_energy_use_scaled = indicator_scale_set(1, 5, kpi10_energy_use), #14
      kpi11a_income_ratio_ref_scaled = indicator_scale_set(
        0.5,
        2,
        kpi11a_income_ratio_ref
      ), #15,
      kpi11a_income_ratio_median_scaled = indicator_scale_set(
        0.5,
        2,
        kpi11a_income_ratio_median
      ), #15
      kpi11b_income_stability_scaled = indicator_scale_set(
        1,
        5,
        kpi11b_income_stability
      ), #16
      kpi11c_income_v_expenditures_scaled = indicator_scale_set(
        0,
        1,
        kpi11c_income_v_expenditures
      ), #17
      kpi11d_income_sufficiency_scaled = indicator_scale_set(
        1,
        5,
        kpi11d_income_sufficiency
      ), #18
      kpi12_yield_gap_ref_scaled = indicator_scale_set(
        0,
        99,
        kpi12_yield_gap_ref
      ), #19,
      kpi12_yield_gap_median_scaled = indicator_scale_set(
        0,
        99,
        kpi12_yield_gap_median
      ), #19
      kpi13a_labour_input_scaled = indicator_scale_main_surveys_rev(
        kpi13a_labour_input
      ), #20
      kpi13b_labour_productivity_scaled = indicator_scale_main_surveys(
        kpi13b_labour_productivity
      ), #21
      kpi14a_climate_resilience_scaled = indicator_scale_set(
        0,
        20,
        kpi14a_climate_resilience
      ), #22
      kpi14b_climate_resilience_scaled = indicator_scale_set(
        1,
        5,
        kpi14b_climate_resilience
      ), #23
      kpi15_diet_diversity_scaled = indicator_scale_set(
        0,
        10,
        kpi15_diet_diversity
      ), #24
      kpi16_farmer_agency_scaled = indicator_scale_set(
        1,
        5,
        kpi16_farmer_agency
      ), #25
      kpi17a_land_security_perception_scaled = indicator_scale_set(
        1,
        5,
        kpi17a_land_security_perception
      ), #26
      kpi17b_land_tenure_scaled = kpi17b_land_tenure, #27
      kpi18_human_wellbeing_scaled = indicator_scale_set(
        1,
        5,
        kpi18_human_wellbeing
      ) #28
    )

  return(performance_indicators)
}

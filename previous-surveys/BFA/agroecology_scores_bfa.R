library(tidyverse)
library(jsonlite)
library(httr)
library(openxlsx)

################################################################################
# IMPORT main_surveys FROM DATABASE
#  - NOTE: this script requires the cleaned dataset to be placed inside the previous-survey-data folder.
#    Any files placed in the previous-survey-data folder should be ignored by git. Please DO NOT push real datasets to this Git repository.
#
################################################################################

source("previous-survey-data/migrate_bfa_to_new_format.R")
source("previous-survey-data/helper_functions.R")

main_surveys <- load_bfa_main_survey_data()
agroecology_scores <- main_surveys %>%
  select(
    household_id,
    kobo_submission_id,
    latitude,
    longitude,
    sector,
    gender,
    birth_year,
    age,
    farm_size
  )

################################################################################
# RECYCLING
################################################################################

recylcing_scores <- function() {
  required_vars <- c(
    "seed_source",
    "organic_fert_source",
    "livestock_source",
    "spawn_source",
    "energy_source"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }
  tmp <- main_surveys %>%
    mutate(
      recycling_1_score = seed_source, # 1
      recycling_1_label = factor(
        # 2
        seed_source,
        levels = c(1:5),
        labels = c(
          "All seeds are purchased from the market (e.g., agrovet, seed stores, farmers' cooperatives, seed suppliers, etc.)",
          "75% of seeds are purchased from the market, the other 25% are self-produced or exchanged",
          "50% of seeds are purchased from the market, the other 50% is self-produced or exchanged",
          "25% of seeds are purchased from the market, the other 75% is self-produced or exchanged",
          "All seeds are self-produced, exchanged with other farmers or managed collectively"
        )
      ),
      recycling_2_score = organic_fert_source, # 3
      recycling_2_label = factor(
        # 4
        organic_fert_source,
        levels = c(1:5),
        labels = c(
          "All manure and compost are purchased from the market",
          "75% of the manure and compost are purchased from the market, the other 25% are self-produced or exchanged",
          "50% of the manure and compost purchased from the market, the other 50% is self-produced or exchanged",
          "25% of the manure and compost are purchased from the market, the other 75% is self-produced or exchanged",
          "All manure and compost are self-produced, exchanged with other farmers or managed collectively"
        )
      ),
      recycling_3_score = livestock_source, # 5
      recycling_3_label = factor(
        # 6
        livestock_source,
        levels = c(1:5),
        labels = c(
          "All animal genetic resources (e.g., chicks, young animals, semen) are purchased from the market",
          "75% of animal genetic resources are purchased from the market, the other 25% are self-produced or exchanged",
          "50% of the breeding are purchased from the market, the other 50% is self-produced or exchanged with neighbouring farms",
          "25% of animal genetic resources are purchased from the market, the other 75% is self-produced or exchanged",
          "All animal genetic resources are self-produced, exchanged with other farmers or managed collectively"
        )
      ),
      recycling_4_score = spawn_source, # 7
      recycling_4_label = factor(
        # 8
        spawn_source,
        levels = c(1:5),
        labels = c(
          "All fish genetic resources (e.g., spawn, fry, fingerling species and varieties) are purchased from the market",
          "75% of fish genetic resources are purchased from the market, the other 25% are self-produced or exchanged",
          "50% of fish genetic resources are purchased from the market, the other 50% is self-produced or exchanged with neighbouring farms",
          "25% of fish genetic resources are purchased from the market, the other 75% is self-produced or exchanged",
          "All fish genetic resources are self-produced, exchanged with other farmers or managed collectively"
        )
      ),
      recycling_5_score = energy_source, # 9
      recycling_5_label = factor(
        # 10
        energy_source,
        levels = c(1:5),
        labels = c(
          "All energy is purchased from the market",
          "75% of energy is purchased from the market, the other 25% is produced on farm/within the agroecosystem or exchanged with other members of the community",
          "50% of energy is purchased from the market, the other 50% produced on farm/within the agroecosystem or exchanged with other members of the community",
          "25% of energy is purchased from the market, the other 75% is self-produced or exchanged",
          "All energy is self-produced, exchanged with other farmers or managed collectively"
        )
      )
    ) %>%
    select(household_id, starts_with("recycling_"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}


################################################################################
#  INPUT REDUCTION
################################################################################

input_reduction_scores <- function() {
  required_vars <- c(
    "sf_methods",
    "sf_methods_2",
    "sf_methods_3",
    "sf_methods_1",
    "pest_methods",
    "pest_methods_2",
    "pest_methods_3",
    "pest_methods_1",
    "dry_feed",
    "fish_feed_type",
    "fish_feed_type_2",
    "fish_feed_type_3",
    "fish_feed_type_1",
    "disease_management",
    "disease_management_1",
    "disease_management_2",
    "disease_management_3",
    "disease_management_4",
    "disease_management_5",
    "disease_management_6",
    "fish_disease_management",
    "fish_disease_management_1",
    "fish_disease_management_2",
    "fish_disease_management_3",
    "fish_disease_management_4",
    "fish_disease_management_5",
    "fish_disease_management_6"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      input_reduction_1_score = case_when(
        # 11
        sf_methods == 3 ~ 5,
        sf_methods == 2 |
          (sf_methods_2 == 1 & sf_methods_3 == 1 & sf_methods_1 == 0) ~ 4,
        (sf_methods_2 == 1 & sf_methods_3 == 1 & sf_methods_1 == 1) |
          (sf_methods_2 == 0 & sf_methods_3 == 1 & sf_methods_1 == 1) ~ 3,
        sf_methods_2 == 1 & sf_methods_3 == 0 & sf_methods_1 == 1 ~ 2,
        sf_methods == 1 ~ 1,
        sf_methods == 0 ~ 5
      ),
      input_reduction_1_label = case_when(
        # 12
        sf_methods == 0 ~ "No ecological practices, chemical or organic fertilizer were applied",
        sf_methods == 3 ~ "Only ecological practices are applied",
        input_reduction_1_score == 4 ~ "Combination of ecological practices and organic fertilizers or manure are applied/ Only organic fertilizers or manure are applied",
        input_reduction_1_score == 3 ~ "Combination of ecological practices and organic fertilizers or manure and/or chemical fertilizers is applied",
        input_reduction_1_score == 2 ~ "Combination of chemical fertilizers and organic fertilizers or manure are applied",
        input_reduction_1_score == 1 ~ "Only chemical fertilizers applied"
      ),
      input_reduction_2_score = case_when(
        # 13
        pest_methods == 3 ~ 5,
        pest_methods == 2 |
          (pest_methods_2 == 1 & pest_methods_3 == 1 & pest_methods_1 == 0) ~ 4,
        (pest_methods_2 == 1 & pest_methods_3 == 1 & pest_methods_1 == 1) |
          (pest_methods_2 == 0 & pest_methods_3 == 1 & pest_methods_1 == 1) ~ 3,
        pest_methods_2 == 1 & pest_methods_3 == 0 & pest_methods_1 == 1 ~ 2,
        pest_methods == 1 ~ 1,
        pest_methods == 0 ~ 5
      ),
      input_reduction_2_label = case_when(
        # 14
        sf_methods == 0 ~ "No ecological practices, chemical or non-chemical pestices were applied",
        sf_methods == 3 ~ "Only ecological practices are applied",
        input_reduction_2_score == 4 ~ "Combination of ecological practices and non-chemical fungicides/pesticides/herbicides /  Only non-chemical fungicides/pesticides/herbicides area applied",
        input_reduction_2_score == 3 ~ "Combination of chemical and non-chemical fungicides/pesticides/herbicides and/or ecological practices",
        input_reduction_2_score == 2 ~ "Combination of chemical and non-chemical fungicides/pesticides/herbicides",
        input_reduction_2_score == 1 ~ "Only chemical fungicides/pesticides/herbicides are applied"
      ),
      input_reduction_3_score = dry_feed, # 15
      inptu_reduction_3_label = factor(
        # 16
        dry_feed,
        levels = c(1:5),
        labels = c(
          "All the time",
          "Often",
          "Sometimes",
          "Rarely",
          "Never"
        )
      ),
      input_reduction_4_score = case_when(
        # 17
        fish_feed_type == 1 ~ 5,
        fish_feed_type == 2 |
          (fish_feed_type_1 == 1 &
            fish_feed_type_2 == 1 &
            fish_feed_type_3 == 0) ~ 4,
        (fish_feed_type_1 == 1 &
          fish_feed_type_2 == 1 &
          fish_feed_type_3 == 1) ~ 3,
        (fish_feed_type_1 == 0 &
          fish_feed_type_2 == 1 &
          fish_feed_type_3 == 1) |
          (fish_feed_type_1 == 1 &
            fish_feed_type_2 == 0 &
            fish_feed_type_3 == 1) ~ 2, # CHECK WITH HOLPA ON THE COMBINATION OF NATURAL AND CHEMICAL
        fish_feed_type == 3 ~ 1
      ),
      input_reduction_4_label = case_when(
        # 18
        input_reduction_4_score == 5 ~ "Only natural feeds used",
        input_reduction_4_score == 4 ~ "Combination of natural and prepared organic feed used. Or only organic",
        input_reduction_4_score == 3 ~ "Combination of natural, preprepared organic feeds and prepared chemical feeds used",
        input_reduction_4_score == 2 ~ "Combination of prepared organic and chemical feeds used",
        input_reduction_4_score == 1 ~ "Only chemical feeds used"
      ),
      input_reduction_5_score = case_when(
        # 19
        # =0
        disease_management == 0 ~ 5,
        # 5 or 6
        (disease_management_5 == 1 | disease_management_6 == 1) &
          (disease_management_1 == 0 &
            disease_management_2 == 0 &
            disease_management_3 == 0 &
            disease_management_4 == 0) ~ 5,
        #  3 or 4 & 5 or 6
        ((disease_management_1 == 0 & disease_management_2 == 0) &
          ((disease_management_3 == 1 | disease_management_4 == 1) &
            (disease_management_5 == 1 | disease_management_6 == 1))) ~ 4,
        # 3 or 4
        (disease_management_3 == 1 | disease_management_4 == 1) &
          (disease_management_1 == 0 &
            disease_management_2 == 0 &
            disease_management_5 == 0 &
            disease_management_6 == 0) ~ 4,
        #  1 or 2 & 5 or 6
        ((disease_management_3 == 0 & disease_management_4 == 0) &
          ((disease_management_1 == 1 | disease_management_2 == 1) &
            (disease_management_5 == 1 | disease_management_6 == 1))) ~ 3,
        # 1 or 2 & 3 or 4 & 5 or 6
        (disease_management_5 == 1 | disease_management_6 == 1) &
          (disease_management_3 == 1 | disease_management_4 == 1) &
          (disease_management_1 == 1 | disease_management_2 == 1) ~ 3,
        # 1 or 2 & 3 or 4
        disease_management_5 == 0 &
          disease_management_6 == 0 &
          ((disease_management_1 == 1 | disease_management_2 == 1) &
            (disease_management_3 == 1 | disease_management_4 == 1)) ~ 2,
        #  1 or 2
        (disease_management_1 == 1 | disease_management_2 == 1) &
          (disease_management_5 == 0 &
            disease_management_6 == 0 &
            disease_management_3 == 0 &
            disease_management_4 == 0) ~ 1
      ),
      input_reduction_5_label = case_when(
        # 20
        disease_management == 0 ~ "No action taken",
        (disease_management_5 == 1 | disease_management_6 == 1) &
          (disease_management_1 == 0 &
            disease_management_2 == 0 &
            disease_management_3 == 0 &
            disease_management_4 == 0) ~ "Only ecological practices/treatments",
        input_reduction_5_score == 4 ~ "Combination of ecological practices/treatments and organic inputs.OR only organic inputs",
        input_reduction_5_score == 3 ~ "Combination of ecological practices/treatments, and chemical and/or organic inputs",
        input_reduction_5_score == 2 ~ "Combination of chemical and organic inputs",
        input_reduction_5_score == 1 ~ "Only chemical inputs"
      ),
      input_reduction_6_score = case_when(
        # 21
        # =0
        fish_disease_management == 0 ~ 5,
        # 5 or 6
        (fish_disease_management_5 == 1 | fish_disease_management_6 == 1) &
          (fish_disease_management_1 == 0 &
            fish_disease_management_2 == 0 &
            fish_disease_management_3 == 0 &
            fish_disease_management_4 == 0) ~ 5,
        #  3 or 4 & 5 or 6
        ((fish_disease_management_1 == 0 & fish_disease_management_2 == 0) &
          ((fish_disease_management_3 == 1 | fish_disease_management_4 == 1) &
            (fish_disease_management_5 == 1 |
              fish_disease_management_6 == 1))) ~ 4,
        # 3 or 4
        (fish_disease_management_3 == 1 | fish_disease_management_4 == 1) &
          (fish_disease_management_1 == 0 &
            fish_disease_management_2 == 0 &
            fish_disease_management_5 == 0 &
            fish_disease_management_6 == 0) ~ 4,
        #  1 or 2 & 5 or 6
        ((fish_disease_management_3 == 0 & fish_disease_management_4 == 0) &
          ((fish_disease_management_1 == 1 | fish_disease_management_2 == 1) &
            (fish_disease_management_5 == 1 |
              fish_disease_management_6 == 1))) ~ 3,
        # 1 or 2 & 3 or 4 & 5 or 6
        (fish_disease_management_5 == 1 | fish_disease_management_6 == 1) &
          (fish_disease_management_3 == 1 | fish_disease_management_4 == 1) &
          (fish_disease_management_1 == 1 | fish_disease_management_2 == 1) ~ 3,
        # 1 or 2 & 3 or 4
        fish_disease_management_5 == 0 &
          fish_disease_management_6 == 0 &
          ((fish_disease_management_1 == 1 | fish_disease_management_2 == 1) &
            (fish_disease_management_3 == 1 |
              fish_disease_management_4 == 1)) ~ 2,
        #  1 or 2
        (fish_disease_management_1 == 1 | fish_disease_management_2 == 1) &
          (fish_disease_management_5 == 0 &
            fish_disease_management_6 == 0 &
            fish_disease_management_3 == 0 &
            fish_disease_management_4 == 0) ~ 1
      ),
      input_reduction_6_label = case_when(
        # 22
        fish_disease_management == 0 ~ "No action taken",
        (fish_disease_management_5 == 1 | fish_disease_management_6 == 1) &
          (fish_disease_management_1 == 0 &
            fish_disease_management_2 == 0 &
            fish_disease_management_3 == 0 &
            fish_disease_management_4 == 0) ~ "Only ecological practices/treatments",
        input_reduction_6_score == 4 ~ "Combination of ecological practices/treatments and organic inputs.OR only organic inputs",
        input_reduction_6_score == 3 ~ "Combination of ecological practices/treatments, and chemical and/or organic inputs",
        input_reduction_6_score == 2 ~ "Combination of chemical and organic inputs",
        input_reduction_6_score == 1 ~ "Only chemical inputs"
      )
    ) %>%
    select(household_id, starts_with("input_reduction"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# SOIL HEALTH
################################################################################

soil_health_score <- function() {
  required_vars <- c("sf_practices_count")

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      soil_health_score = case_when(
        # 23
        sf_practices_count >= 4 ~ 5,
        sf_practices_count == 3 ~ 4,
        sf_practices_count == 2 ~ 3,
        sf_practices_count == 1 ~ 2,
        sf_practices_count == 0 ~ 1,
        is.na(sf_practices_count) ~ 1,
      ),
      soil_health_label = case_when(
        # 24
        soil_health_score == 5 ~ "Implementing 4 or more practices",
        soil_health_score == 4 ~ "Implementing 3 practices",
        soil_health_score == 3 ~ "Implementing 2 practices",
        soil_health_score == 2 ~ "Implementing 1 practice",
        soil_health_score == 1 ~ "Not implementing any practice"
      )
    ) %>%
    select(household_id, soil_health_score, soil_health_label)

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# ANIMAL HEALTH
################################################################################

animal_health_scores <- function() {
  required_vars <- c(
    "animal_health",
    "animal_health_management_count",
    "fish_land_practice_count"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      animal_health_1_score = animal_health, # 25
      animal_health_1_label = factor(
        # 26
        animal_health,
        levels = c(1:5),
        labels = c(
          "Animals suffer from hunger and thirst, stress and diseases all year long, and are slaughtered without avoiding unnecessary pain",
          "Animals suffer periodically/seasonally from hunger and thirst, stress or diseases, and are slaughtered without avoiding unnecessary pain",
          "Animals do not suffer from hunger or thirst, but suffer from stress, may be prone to diseases and can suffer from pain at slaughter",
          "Animals do not suffer from hunger, thirst or diseases but can experience stress, especially at slaughter",
          "Animals do not suffer from stress, hunger, thirst, pain, or diseases, and are slaughtered in a way to avoid unnecessary pain"
        )
      ),
      animal_health_2_score = case_when(
        # 27
        animal_health_management_count >= 4 ~ 5,
        animal_health_management_count == 3 ~ 4,
        animal_health_management_count == 2 ~ 3,
        animal_health_management_count == 1 ~ 2,
        animal_health_management_count == 0 ~ 1,
      ),
      animal_health_2_label = case_when(
        # 28
        animal_health_2_score == 5 ~ "Implementing 4 or more practices",
        animal_health_2_score == 4 ~ "Implementing 3 practices",
        animal_health_2_score == 3 ~ "Implementing 2 practices",
        animal_health_2_score == 2 ~ "Implementing 1 practice",
        animal_health_2_score == 1 ~ "Not implementing any practice"
      ),
      animal_health_3_score = case_when(
        # 29
        fish_land_practice_count >= 4 ~ 5,
        fish_land_practice_count == 3 ~ 4,
        fish_land_practice_count == 2 ~ 3,
        fish_land_practice_count == 1 ~ 2,
        fish_land_practice_count == 0 ~ 1,
      ),
      animal_health_3_label = case_when(
        # 30
        animal_health_3_score == 5 ~ "Implementing 4 or more practices",
        animal_health_3_score == 4 ~ "Implementing 3 practices",
        animal_health_3_score == 3 ~ "Implementing 2 practices",
        animal_health_3_score == 2 ~ "Implementing 1 practice",
        animal_health_3_score == 1 ~ "Not implementing any practice"
      )
    ) %>%
    select(household_id, animal_health_1_score:animal_health_3_label)

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# BIODIVERSITY
################################################################################

div_score <- function(var) {
  required_vars <- c(
    "crops_count",
    "livestock_count",
    "fish_count",
    "tree_diversity",
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

  scores <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} == "high" ~ 5,
        {{ var }} == "medium" ~ 3.67,
        {{ var }} == "low" ~ 2.33,
        {{ var }} == "none" ~ 1
      )
    )

  return(scores[[1]])
}

div_labels <- function(var) {
  required_vars <- c(
    "crops_count",
    "livestock_count",
    "fish_count",
    "tree_diversity",
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

  labels <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} == "high" ~ "High: five or more species with different heights, woodiness or flowering seasons",
        {{ var }} == "medium" ~ "Medium: two to four species",
        {{ var }} == "low" ~ "Low: only one species",
        {{ var }} == "none" ~ "None"
      )
    )

  return(labels[[1]])
}

biodiversity_scores <- function() {
  required_vars <- c(
    "crops_count",
    "livestock_count",
    "fish_count",
    "tree_diversity",
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

  print(main_surveys$crops_count)

  tmp <- main_surveys %>%
    mutate(
      biodiversity_1_score = case_when(
        # 31
        crops_count <= quantile(crops_count, 0.2, na.rm = TRUE) ~ 1,
        crops_count <= quantile(crops_count, 0.4, na.rm = TRUE) ~ 2,
        crops_count <= quantile(crops_count, 0.6, na.rm = TRUE) ~ 3,
        crops_count <= quantile(crops_count, 0.8, na.rm = TRUE) ~ 4,
        crops_count > quantile(crops_count, 0.8, na.rm = TRUE) ~ 5
      ),
      biodiversity_1_label = factor(
        # 32
        biodiversity_1_score,
        levels = c(1:5),
        labels = c(
          "0 - 20th percentile (low diversity)",
          "21st - 40th percentile",
          "41st - 60th percentile (median diversity)",
          "61st - 80th percentile",
          "81st - 100th percentile (high diversity)"
        )
      ),
      biodiversity_2_score = case_when(
        # 33
        livestock_count <= quantile(livestock_count, 0.2, na.rm = TRUE) ~ 1,
        livestock_count <= quantile(livestock_count, 0.4, na.rm = TRUE) ~ 2,
        livestock_count <= quantile(livestock_count, 0.6, na.rm = TRUE) ~ 3,
        livestock_count <= quantile(livestock_count, 0.8, na.rm = TRUE) ~ 4,
        livestock_count > quantile(livestock_count, 0.8, na.rm = TRUE) ~ 5
      ),
      biodiversity_2_label = factor(
        # 34
        biodiversity_2_score,
        levels = c(1:5),
        labels = c(
          "0 - 20th percentile (low diversity)",
          "21st - 40th percentile",
          "41st - 60th percentile (median diversity)",
          "61st - 80th percentile",
          "81st - 100th percentile (high diversity)"
        )
      ),
      biodiversity_3_score = case_when(
        # 35
        fish_count <= quantile(fish_count, 0.2, na.rm = TRUE) ~ 1,
        fish_count <= quantile(fish_count, 0.4, na.rm = TRUE) ~ 2,
        fish_count <= quantile(fish_count, 0.6, na.rm = TRUE) ~ 3,
        fish_count <= quantile(fish_count, 0.8, na.rm = TRUE) ~ 4,
        fish_count > quantile(fish_count, 0.8, na.rm = TRUE) ~ 5
      ),
      biodiversity_3_label = factor(
        # 36
        biodiversity_3_score,
        levels = c(1:5),
        labels = c(
          "0 - 20th percentile (low diversity)",
          "21st - 40th percentile",
          "41st - 60th percentile (median diversity)",
          "61st - 80th percentile",
          "81st - 100th percentile (high diversity)"
        )
      ),
      biodiversity_4_score = div_score(tree_diversity), # 37
      biodiversity_4_label = div_labels(tree_diversity), # 38
      biodiversity_5_score = div_score(bushland_diversity), # 39
      biodiversity_5_label = div_labels(bushland_diversity), # 40
      biodiversity_6_score = div_score(fallow_land_diversity), # 41
      biodiversity_6_label = div_labels(fallow_land_diversity), # 42
      biodiversity_7_score = div_score(hedgerows_diversity), # 43
      biodiversity_7_label = div_labels(hedgerows_diversity), # 44
      biodiversity_8_score = div_score(grassland_diversity), # 45
      biodiversity_8_label = div_labels(grassland_diversity), # 46
      biodiversity_9_score = div_score(forest_patches_diversity), # 47
      biodiversity_9_label = div_labels(forest_patches_diversity), # 48
      biodiversity_10_score = div_score(wetlands_diversity), # 49
      biodiversity_10_label = div_labels(wetlands_diversity), # 50
      biodiversity_11_score = div_score(woodlots_diversity), # 51
      biodiversity_11_label = div_labels(woodlots_diversity) # 52
    ) %>%
    select(household_id, starts_with("biodiversity"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# SYNERGY
################################################################################

syn_score <- function(var) {
  required_vars <- c(
    "ecological_practices_count",
    "sf_practices_count",
    "pd_practices_count",
    "grazing_practice_count",
    "fish_land_practice_count",
    "relationship_actions_count"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  scores <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} >= 4 ~ 5,
        {{ var }} == 3 ~ 4,
        {{ var }} == 2 ~ 3,
        {{ var }} == 1 ~ 2,
        {{ var }} == 0 ~ 1,
      )
    )

  return(scores[[1]])
}

syn_labels <- function(var) {
  required_vars <- c(
    "ecological_practices_count",
    "sf_practices_count",
    "pd_practices_count",
    "grazing_practice_count",
    "fish_land_practice_count",
    "relationship_actions_count"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  labels <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} >= 4 ~ "Implementing 4 or more practices",
        {{ var }} == 3 ~ "Implementing 3 practices",
        {{ var }} == 2 ~ "Implementing 2 practices",
        {{ var }} == 1 ~ "Implementing 1 practice",
        {{ var }} == 0 ~ "Not implementing any practice"
      )
    )

  return(labels[[1]])
}

synergy_scores <- function() {
  required_vars <- c(
    "ecological_practices_count",
    "sf_practices_count",
    "pd_practices_count",
    "grazing_practice_count",
    "fish_land_practice_count",
    "relationship_actions_count"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      synergy_1_score = syn_score(ecological_practices_count), # 53
      synergy_1_label = syn_labels(ecological_practices_count), # 54
      synergy_2_score = syn_score(sf_practices_count), # 55
      synergy_2_label = syn_labels(sf_practices_count), # 56
      synergy_3_score = syn_score(pd_practices_count), # 57
      synergy_3_label = syn_labels(pd_practices_count), # 58
      synergy_4_score = syn_score(grazing_practice_count), # 59
      synergy_4_label = syn_labels(grazing_practice_count), # 60
      synergy_5_score = syn_score(fish_land_practice_count), # 61
      synergy_5_label = syn_labels(fish_land_practice_count), # 62
      synergy_6_score = syn_score(relationship_actions_count), # 63
      synergy_6_label = syn_labels(relationship_actions_count) # 64
    ) %>%
    select(household_id, starts_with("synergy_"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# ECONOMIC DIVERSIFICATION
################################################################################

economic_div_score <- function() {
  required_vars <- c("income_count")

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      # 65
      economic_diversification_score = case_when(
        income_count >= 5 ~ 5,
        income_count == 4 ~ 4,
        income_count == 3 ~ 3,
        income_count == 2 ~ 2,
        income_count == 1 ~ 1
      ),
      # 66
      economic_diversification_label = case_when(
        income_count >= 5 ~ "Five or more methods of income generation",
        income_count == 4 ~ "Four methods of income generation",
        income_count == 3 ~ "Three methods of income generation",
        income_count == 2 ~ "Two methods of income generation",
        income_count == 1 ~ "One method of income generation"
      )
    ) %>%
    select(household_id, starts_with("economic_diversification"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# CO-CREATION OF KNOWLEDGE
################################################################################

cck_score <- function(var) {
  required_vars <- c(
    "share_extension_workers",
    "share_consumers",
    "share_traders",
    "share_govt",
    "share_ngos",
    "share_farmers",
    "share_researchers"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  scores <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} >= 5 ~ 5,
        {{ var }} == 4 ~ 4,
        {{ var }} %in% c(2, 3) ~ 3,
        {{ var }} == 1 ~ 2,
        {{ var }} == 0 ~ 1,
      )
    )

  return(scores[[1]])
}

cck_labels <- function(var) {
  required_vars <- c(
    "share_extension_workers",
    "share_consumers",
    "share_traders",
    "share_govt",
    "share_ngos",
    "share_farmers",
    "share_researchers"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  labels <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} >= 5 ~ "5 or more times per year",
        {{ var }} == 4 ~ "4 times per year",
        {{ var }} %in% c(2, 3) ~ "2 to 3 times per year",
        {{ var }} == 1 ~ "1 time per year",
        {{ var }} == 0 ~ "Never"
      )
    )

  return(labels[[1]])
}

cc_knowledge_scores <- function() {
  required_vars <- c(
    "share_extension_workers",
    "share_consumers",
    "share_traders",
    "share_govt",
    "share_ngos",
    "share_farmers",
    "share_researchers"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      co_creation_knowledge_1_score = cck_score(share_extension_workers), # 67
      co_creation_knowledge_1_label = cck_labels(share_extension_workers), # 68
      co_creation_knowledge_2_score = cck_score(share_consumers), # 69
      co_creation_knowledge_2_label = cck_labels(share_consumers), # 70
      co_creation_knowledge_3_score = cck_score(share_traders), # 71
      co_creation_knowledge_3_label = cck_labels(share_traders), # 72
      co_creation_knowledge_4_score = cck_score(share_govt), # 73
      co_creation_knowledge_4_label = cck_labels(share_govt), # 74
      co_creation_knowledge_5_score = cck_score(share_ngos), # 75
      co_creation_knowledge_5_label = cck_labels(share_ngos), # 76
      co_creation_knowledge_6_score = cck_score(share_farmers), # 77
      co_creation_knowledge_6_label = cck_labels(share_farmers), # 78
      co_creation_knowledge_7_score = cck_score(share_researchers), # 79
      co_creation_knowledge_7_label = cck_labels(share_researchers) # 80
    ) %>%
    select(household_id, starts_with("co_creation_knowledge"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# SOCIAL VALUES AND DIET
################################################################################

diet_labels <- function(var) {
  required_vars <- c(
    "access_healthy_food",
    "access_diverse_food",
    "access_seasonal_food",
    "access_traditional_food"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  labels <- main_surveys %>%
    select({{ var }}) %>%
    mutate(
      {{ var }} := case_when(
        {{ var }} == 5 ~ "Good access",
        {{ var }} == 4 ~ "Fairly good",
        {{ var }} == 3 ~ "Moderate access",
        {{ var }} == 2 ~ "Limited access",
        {{ var }} == 1 ~ "No access at all"
      )
    )

  return(labels[[1]])
}

diet_scores <- function() {
  required_vars <- c(
    "access_healthy_food",
    "access_diverse_food",
    "access_seasonal_food",
    "access_traditional_food"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      social_values_diet_1_score = access_healthy_food, # 81
      social_values_diet_1_label = diet_labels(access_healthy_food), # 82
      social_values_diet_2_score = access_diverse_food, # 83
      social_values_diet_2_label = diet_labels(access_diverse_food), # 84
      social_values_diet_3_score = access_seasonal_food, # 85
      social_values_diet_3_label = diet_labels(access_seasonal_food), # 86
      social_values_diet_4_score = access_traditional_food, # 87
      social_values_diet_4_label = diet_labels(access_traditional_food) # 88
    ) %>%
    select(household_id, starts_with("social_values_diet"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# FAIRNESS
################################################################################

fairness_scores <- function() {
  tmp_a <- main_surveys %>%
    mutate(
      ## Fairness 1 = Crops
      fairness_1_score = crop_fair_price,
      fairness_1_label = case_when(
        crop_fair_price == 5 ~ "Always get a fair price",
        crop_fair_price == 4 ~ "Usually get a fair price, depending on the product",
        crop_fair_price == 3 ~ "Occasionally get a fair price, depending on the product",
        crop_fair_price == 2 ~ "Rarely get a fair price",
        crop_fair_price == 1 ~ "Never get a fair price./I don't know"
      ),

      ## Fairness 2 = Livestock
      fairness_2_score = livestock_fair_price,
      fairness_2_label = case_when(
        livestock_fair_price == 5 ~ "Always get a fair price",
        livestock_fair_price == 4 ~ "Usually get a fair price, depending on the product",
        livestock_fair_price == 3 ~ "Occasionally get a fair price, depending on the product",
        livestock_fair_price == 2 ~ "Rarely get a fair price",
        livestock_fair_price == 1 ~ "Never get a fair price./I don't know"
      ),
      ## Fairness 3 = Fish
      fairness_3_score = fish_fair_price,
      fairness_3_label = case_when(
        fish_fair_price == 5 ~ "Always get a fair price",
        fish_fair_price == 4 ~ "Usually get a fair price, depending on the product",
        fish_fair_price == 3 ~ "Occasionally get a fair price, depending on the product",
        fish_fair_price == 2 ~ "Rarely get a fair price",
        fish_fair_price == 1 ~ "Never get a fair price./I don't know"
      ),
      ## Fairness 4 = Trees
      fairness_4_score = trees_fair_price,
      fairness_4_label = case_when(
        trees_fair_price == 5 ~ "Always get a fair price",
        trees_fair_price == 4 ~ "Usually get a fair price, depending on the product",
        trees_fair_price == 3 ~ "Occasionally get a fair price, depending on the product",
        trees_fair_price == 2 ~ "Rarely get a fair price",
        trees_fair_price == 1 ~ "Never get a fair price./I don't know"
      ),
      ## Fairness 5 = Honey
      fairness_5_score = honey_fair_price,
      fairness_5_label = case_when(
        honey_fair_price == 5 ~ "Always get a fair price",
        honey_fair_price == 4 ~ "Usually get a fair price, depending on the product",
        honey_fair_price == 3 ~ "Occasionally get a fair price, depending on the product",
        honey_fair_price == 2 ~ "Rarely get a fair price",
        honey_fair_price == 1 ~ "Never get a fair price./I don't know"
      ),
      ## Fairness 6 Score = Other
      fairness_6_score = other_fair_price,
      fairness_6_label = case_when(
        other_fair_price == 5 ~ "Always get a fair price",
        other_fair_price == 4 ~ "Usually get a fair price, depending on the product",
        other_fair_price == 3 ~ "Occasionally get a fair price, depending on the product",
        other_fair_price == 2 ~ "Rarely get a fair price",
        other_fair_price == 1 ~ "Never get a fair price./I don't know"
      )
    ) %>%
    select(household_id, starts_with("fairness"))

  agroecology_scores <- agroecology_scores %>%
    left_join(tmp_a, by = c("household_id" = "household_id"))

  return(agroecology_scores)
}


################################################################################
# CONNECTIVITY
################################################################################

# CHECK DEFINITION FOR OTHERS

connectivity_scores <- function() {
  required_vars <- c(
    "crop_sell_to",
    "crop_sell_to_direct_to_consumer",
    "crop_sell_to_trader_or_supermarket",
    "crop_sell_to_middle_man_aggregator",
    "crop_sell_to_cooperative",
    "crop_sell_to_other",

    "livestock_sell_to",
    "livestock_sell_to_direct_to_consumer",
    "livestock_sell_to_trader_or_supermarket",
    "livestock_sell_to_middle_man_aggregator",
    "livestock_sell_to_cooperative",
    "livestock_sell_to_other",

    "fish_sell_to",
    "fish_sell_to_direct_to_consumer",
    "fish_sell_to_trader_or_supermarket",
    "fish_sell_to_middle_man_aggregator",
    "fish_sell_to_cooperative",
    "fish_sell_to_other",

    "trees_sell_to",
    "trees_sell_to_direct_to_consumer",
    "trees_sell_to_trader_or_supermarket",
    "trees_sell_to_middle_man_aggregator",
    "trees_sell_to_cooperative",
    "trees_sell_to_other",

    "honey_sell_to",
    "honey_sell_to_direct_to_consumer",
    "honey_sell_to_trader_or_supermarket",
    "honey_sell_to_middle_man_aggregator",
    "honey_sell_to_cooperative",
    "honey_sell_to_other",

    "other_sell_to",
    "other_sell_to_direct_to_consumer",
    "other_sell_to_trader_or_supermarket",
    "other_sell_to_middle_man_aggregator",
    "other_sell_to_cooperative",
    "other_sell_to_other"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      ## Connectivity 1 = Crops
      connectivity_1_score = case_when(
        str_detect(crop_sell_to, "direct_to_consumer") ~ 5,
        str_detect(crop_sell_to, "trader_or_supermarket") |
          str_detect(crop_sell_to, "cooperative") ~ 4,
        str_detect(crop_sell_to, "retailers") ~ 3,
        str_detect(crop_sell_to, "middle_man_aggregator") ~ 2,
        is.na(crop_sell_to) ~ 1,
        crop_sell_to_other == 1 ~ 4
      ),
      connectivity_1_label = case_when(
        connectivity_1_score == 5 ~ "Directly to consumers",
        connectivity_1_score == 4 ~ "To farmers organisation/cooperative",
        connectivity_1_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_1_score == 2 ~ "To a middle man/aggregator",
        connectivity_1_score == 1 ~ "Does not sell its products"
      ),

      ## Connectivity 2 = Livestock
      connectivity_2_score = case_when(
        str_detect(livestock_sell_to, "direct_to_consumer") ~ 5,
        str_detect(livestock_sell_to, "trader_or_supermarket") |
          str_detect(livestock_sell_to, "cooperative") ~ 4,
        str_detect(livestock_sell_to, "retailers") ~ 3,
        str_detect(livestock_sell_to, "middle_man_aggregator") ~ 2,
        is.na(livestock_sell_to) ~ 1,
        livestock_sell_to_other == 1 ~ 4
      ),
      connectivity_2_label = case_when(
        connectivity_2_score == 5 ~ "Directly to consumers",
        connectivity_2_score == 4 ~ "To farmers organisation/cooperative",
        connectivity_2_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_2_score == 2 ~ "To a middle man/aggregator",
        connectivity_2_score == 1 ~ "Does not sell its products"
      ),

      ## Connectivity 3 = Fish

      connectivity_3_score = case_when(
        str_detect(fish_sell_to, "direct_to_consumer") ~ 5,
        str_detect(fish_sell_to, "trader_or_supermarket") |
          str_detect(fish_sell_to, "cooperative") ~ 4,
        str_detect(fish_sell_to, "retailers") ~ 3,
        str_detect(fish_sell_to, "middle_man_aggregator") ~ 2,
        is.na(fish_sell_to) ~ 1,
        fish_sell_to_other == 1 ~ 4
      ),
      connectivity_3_label = case_when(
        connectivity_3_score == 5 ~ "Directly to consumers",
        connectivity_3_score == 4 ~ "To farmers organisation/cooperative",
        connectivity_3_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_3_score == 2 ~ "To a middle man/aggregator",
        connectivity_3_score == 1 ~ "Does not sell its products"
      ),

      ## Connectivity 4 = Trees

      connectivity_4_score = case_when(
        str_detect(trees_sell_to, "direct_to_consumer") ~ 5,
        str_detect(trees_sell_to, "trader_or_supermarket") |
          str_detect(trees_sell_to, "cooperative") ~ 4,
        str_detect(trees_sell_to, "retailers") ~ 3,
        str_detect(trees_sell_to, "middle_man_aggregator") ~ 2,
        is.na(trees_sell_to) ~ 1,

        trees_sell_to_other == 1 ~ 4
      ),
      connectivity_4_label = case_when(
        connectivity_4_score == 5 ~ "Directly to consumers",
        connectivity_4_score == 4 ~ "To farmers organisation/cooperative",
        connectivity_4_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_4_score == 2 ~ "To a middle man/aggregator",
        connectivity_4_score == 1 ~ "Does not sell its products"
      ),

      ## Connectivity 5 = Honey

      connectivity_5_score = case_when(
        str_detect(honey_sell_to, "direct_to_consumer") ~ 5,
        str_detect(honey_sell_to, "trader_or_supermarket") |
          str_detect(honey_sell_to, "cooperative") ~ 4,
        str_detect(honey_sell_to, "retailers") ~ 3,
        str_detect(honey_sell_to, "middle_man_aggregator") ~ 2,
        is.na(honey_sell_to) ~ 1,
        honey_sell_to_other == 1 ~ 4
      ),
      connectivity_5_label = case_when(
        connectivity_5_score == 5 ~ "Directly to consumers",
        connectivity_5_score == 4 ~ "To farmers organisation/cooperative",
        connectivity_5_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_5_score == 2 ~ "To a middle man/aggregator",
        connectivity_5_score == 1 ~ "Does not sell its products"
      ),

      ## Connectivity 6 = Other products

      connectivity_6_score = case_when(
        str_detect(other_sell_to, "direct_to_consumer") ~ 5,
        str_detect(other_sell_to, "trader_or_supermarket") |
          str_detect(other_sell_to, "cooperative") ~ 4,
        str_detect(other_sell_to, "retailers") ~ 3,
        str_detect(other_sell_to, "middle_man_aggregator") ~ 2,
        is.na(other_sell_to) ~ 1,
        other_sell_to_other == 1 ~ 4
      ),
      connectivity_6_label = case_when(
        connectivity_6_score == 5 ~ "Directly to consumers",
        connectivity_6_score == 4 ~ "To farmers organisation/cooperative",
        connectivity_6_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_6_score == 2 ~ "To a middle man/aggregator",
        connectivity_6_score == 1 ~ "Does not sell its products"
      )
    ) %>%
    select(household_id, starts_with("connectivity"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}


################################################################################
# LAND AND NATURAL RESOURCE GOVERNANCE
################################################################################

governance_scores <- function() {
  required_vars <- c(
    "activities_land_management",
    "influence_land_management",
    "land_management_view"
  )

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      governance_1_score = as.numeric(activities_land_management), # 113
      governance_1_label = factor(
        # 114
        activities_land_management,
        levels = c(1:5),
        labels = c(
          "Never participates",
          "Rarely participates",
          "Sometimes participates",
          "Most of the times participates",
          "Always participates"
        )
      ),
      governance_2_score = as.numeric(influence_land_management), # 115
      governance_2_label = factor(
        ## 116
        influence_land_management,
        levels = c(1:5),
        labels = c(
          "Did not contribute to any decision",
          "Contribute to few decisions",
          "Contribute to some decisions",
          "Contribute to almost all the decisions",
          "Contribute to all the decisions"
        )
      ),
      governance_3_score = as.numeric(land_management_view), # 117
      governance_3_label = factor(
        # 118
        land_management_view,
        levels = c(1:5),
        labels = c(
          "Not at all well-managed",
          "Poorly managed",
          "Moderately managed",
          "Well-managed",
          "Extremely well-managed"
        )
      )
    ) %>%
    select(household_id, starts_with("governance_"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# PARTICIPATION
################################################################################

participation_scores <- function() {
  required_vars <- c("association_effectiveness")

  for (i in required_vars) {
    if (i %!in% colnames(main_surveys)) {
      main_surveys <- main_surveys %>%
        mutate(!!i := NA)
    }
  }

  tmp <- main_surveys %>%
    mutate(
      participation_score = ifelse(
        # 119
        association_effectiveness == 999 | association_effectiveness == "i-dont-know",
        1,
        association_effectiveness
      ),
      participation_label = case_when(
        # 120
        association_effectiveness == 5 ~ "Associations/organizations demonstrate exceptional effectiveness in supporting farmers' business ventures,
        offering comprehensive assistance, fostering growth, and ensuring long-term success",
        association_effectiveness == 4 ~ "Associations/organizations play a significant role in supporting farmers' businesses,
        providing valuable resources, market opportunities, and essential services",
        association_effectiveness == 3 ~ "Associations/organizations offer satisfactory support to farmers,
        aiding them in various aspects of their businesses (e.g., market access, information sharing, and capacity development)",
        association_effectiveness == 2 ~ "Associations/organizations provide limited support to farmers in business,
        with marginal impact on their overall success",
        association_effectiveness == 1 ~ "Associations/organizations offer no support to farmers' businesses.",
        association_effectiveness == 999 | association_effectiveness == "i-dont-know" ~ "I don't know"
      )
    ) %>%
    select(household_id, starts_with("participation_"))

  agroecology_scores <- left_join(
    agroecology_scores,
    tmp
  )

  return(agroecology_scores)
}

################################################################################
# CREATE AGROECOLOGY SCORE DATBASE TABLE
################################################################################

agroecology_scores <- recylcing_scores()
agroecology_scores <- input_reduction_scores()
agroecology_scores <- soil_health_score()
agroecology_scores <- animal_health_scores()
agroecology_scores <- biodiversity_scores()
agroecology_scores <- synergy_scores()
agroecology_scores <- economic_div_score()
agroecology_scores <- cc_knowledge_scores()
agroecology_scores <- diet_scores()
agroecology_scores <- fairness_scores()
agroecology_scores <- connectivity_scores()
agroecology_scores <- governance_scores()
agroecology_scores <- participation_scores()


# Final scores

agroecology_scores <- agroecology_scores %>%
  rowwise() %>%
  mutate(
    overall_recycling_score = median(
      c(
        recycling_1_score,
        recycling_2_score,
        recycling_3_score,
        recycling_4_score,
        recycling_5_score
      ),
      na.rm = TRUE
    ),
    overall_input_reduction_score = median(
      c(
        input_reduction_1_score,
        input_reduction_2_score,
        input_reduction_3_score,
        input_reduction_4_score,
        input_reduction_5_score,
        input_reduction_6_score
      ),
      na.rm = TRUE
    ),
    overall_soil_health_score = soil_health_score,
    overall_animal_health_score = median(
      c(
        animal_health_1_score,
        animal_health_2_score,
        animal_health_3_score
      ),
      na.rm = TRUE
    ),
    overall_biodiversity_score = median(
      c(
        biodiversity_1_score,
        biodiversity_2_score,
        biodiversity_3_score,
        biodiversity_4_score,
        biodiversity_5_score,
        biodiversity_6_score,
        biodiversity_7_score,
        biodiversity_8_score,
        biodiversity_9_score,
        biodiversity_10_score,
        biodiversity_11_score
      ),
      na.rm = TRUE
    ),
    overall_synergy_score = median(
      c(
        ## Temporarily remove the NA values - na.rm should work; need to explore why it's not here.
        ##synergy_1_score,
        ##synergy_2_score,
        synergy_3_score,
        ##synergy_4_score,
        synergy_5_score,
        synergy_6_score
      ),
      na.rm = TRUE
    ),
    overall_economic_diversification_score = economic_diversification_score,
    overall_co_creation_knowledge_score = median(
      c(
        co_creation_knowledge_1_score,
        co_creation_knowledge_2_score,
        co_creation_knowledge_3_score,
        co_creation_knowledge_4_score,
        co_creation_knowledge_5_score,
        co_creation_knowledge_6_score,
        co_creation_knowledge_7_score
      ),
      na.rm = TRUE
    ),
    overall_social_values_diet_score = median(
      c(
        social_values_diet_1_score,
        social_values_diet_2_score,
        social_values_diet_3_score,
        social_values_diet_4_score
      ),
      na.rm = TRUE
    ),
    overall_fairness_score = median(
      c(
        fairness_1_score,
        fairness_2_score,
        fairness_3_score,
        fairness_4_score,
        fairness_5_score,
        fairness_6_score
      ),
      na.rm = TRUE
    ),
    overall_connectivity_score = median(
      c(
        connectivity_1_score,
        connectivity_2_score,
        connectivity_3_score,
        connectivity_4_score,
        connectivity_5_score,
        connectivity_6_score
      ),
      na.rm = TRUE
    ),
    overall_governance_score = median(
      c(
        governance_1_score,
        governance_2_score,
        governance_3_score
      ),
      na.rm = TRUE
    ),
    overall_participation_score = as.numeric(participation_score),
  ) %>%
  ungroup()



# ################################################################################
# # WRITE TABLE TO OUTPUT EXCEL FILE
# ################################################################################

## add country
agroecology_scores <- agroecology_scores %>%
  mutate(country_id = "854", country_code = "BFA")


write.xlsx(agroecology_scores, file = "output/agroecology_scores_bfa.xlsx", rowNames = FALSE)


# ################################################################################
# # WRITE TABLE TO OUTPUT DATABASE
# ################################################################################

#### This is used to write data back to the main HOLPA Platform database. It is not needed unless you are also using a MySQL database for data storage locally.
#### For most local use, the Excel export above is sufficient.

# source("previous-survey-data/get_db_connection.R")
# agroecology_scores_db <- dbWriteTable(con, "previous_agroecology_scores", agroecology_scores, overwrite = TRUE)

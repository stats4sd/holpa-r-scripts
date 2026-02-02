library(readxl)
library(tidyverse)

################################################################################
# IMPORT main_surveys FROM DATABASE
#  - NOTE: this script requires the cleaned dataset to be placed inside the previous-survey-data folder
#    This folder is ignored by Git deliberately, because the data is not currently shareable. If you are working with the Burkina Faso data, please first obtain the data from the relevant sources and place it in the previous-survey-data folder.
#
################################################################################

## Double check the filename for the clean household survey data file.
bfa_data_filename <- 'bfa_holpa_household_survey_clean.xlsx'


### Import and convert the 'main survey' data (not repeat groups)
load_bfa_main_survey_data <- function() {
  bfa_household_data <- read_excel(
    paste0('previous-survey-data/', bfa_data_filename),
    sheet = 'HOLPA_global_household_survey'
  )

  ## remove first row which contains variable labels
  bfa_household_data <- bfa_household_data[-1, ]

  required_vars <- c(
    ## Context Variables
    "kobo_submission_id",
    "household_id",
    "latitude",
    "longitude",
    "sector",
    "commune",
    "gender",
    "birth_year",
    "age",
    "land_owned",
    "land_leased",
    "land_use_rights",
    "farm_size",

    ### 1_recycling
    "seed_source",
    "organic_fert_source",
    "livestock_source",
    "spawn_source",
    "energy_source",

    ### 2_input_reduction
    "dry_feed",

    "sf_methods_1",
    "sf_methods_2",
    "sf_methods_3",
    "sf_methods_0",

    "pest_methods_1",
    "pest_methods_2",
    "pest_methods_3",
    "pest_methods_0",

    "fish_feed_type_1",
    "fish_feed_type_2",
    "fish_feed_type_3",
    "fish_feed_type_0",

    "disease_management_0",
    "disease_management_1",
    "disease_management_2",
    "disease_management_3",
    "disease_management_4",
    "disease_management_5",
    "disease_management_6",

    "fish_disease_management_0",
    "fish_disease_management_1",
    "fish_disease_management_2",
    "fish_disease_management_3",
    "fish_disease_management_4",
    "fish_disease_management_5",
    "fish_disease_management_6",

    ### 3_soil_health

    "sf_practices_count",

    ### 4_animal_health:
    "animal_health",

    "animal_health_management_count",
    "fish_land_practice_count",

    ### 5_biodiversity:
    "bushland_diversity",
    "fallow_land_diversity",
    "hedgerows_diversity",
    "grassland_diversity",
    "forest_patches_diversity",
    "wetlands_diversity",
    "woodlots_diversity",
    "tree_diversity",

    "livestock_count",
    "crops_count",
    "fish_count",

    ### 6- synergy:
    ## TODO: get from _3_3_3_2_begin_repeat worksheet

    # count number of practices selected
    "sf_practices_count",
    "pd_practices_count",
    "grazing_practice_count",
    "fish_land_practice_count",
    "relationship_actions_count",

    ### 7- economic_diversification:
    "income_count",

    ### 8. Knowledge
    ### Integers; often very high (some maxed out "other farmers"),
    "share_extension_workers",
    "share_consumers",
    "share_traders",
    "share_govt",
    "share_ngos",
    "share_farmers",
    "share_researchers",

    ### 9_social_values:
    "access_healthy_food",
    "access_diverse_food",
    "access_seasonal_food",
    "access_traditional_food",

    ### 10_fairness:
    "crop_fair_price",
    "livestock_fair_price",
    "fish_fair_price",
    "trees_fair_price",
    "honey_fair_price",
    "other_fair_price",

    ### 11_connectivity:
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
    "other_sell_to_other",

    # Ignoring these as I've no idea how these "sales" % values are used in the calculations.

    ### 12_governance:
    "activities_land_management",
    "influence_land_management",
    "land_management_view",

    ### 13_participation:
    "association_effectiveness"
  )

  required_original_vars <- c(
    ## Context
    "_id",
    "__1_3_latitude",
    "__1_3_longitude",
    "_1_2_1_1",
    "_1_2_1_2",
    "_1_2_1_9",
    "_1_2_1_8",

    ## Land Owned
    "_1_4_1_1_1",
    "_1_4_1_1_2",
    "_1_4_1_1_3",

    # 1_recycling:
    "_2_8_1_1",
    "_2_8_2_1",
    "_2_8_3_1",
    "_2_8_5_1",
    "_2_8_4_5",

    # 2_input_reduction:
    "_3_4_4_2",

    ## Select multiples
    "_1_4_3_1",
    "_1_4_3_1/0",
    "_1_4_3_1/1",
    "_1_4_3_1/2",
    "_1_4_3_1/3",

    "_1_4_3_5",
    "_1_4_3_5/0",
    "_1_4_3_5/1",
    "_1_4_3_5/2",
    "_1_4_3_5/3",

    "_2_8_5_3",
    "_2_8_5_3/0",
    "_2_8_5_3/1",
    "_2_8_5_3/2",
    "_2_8_5_3/3",

    "_1_4_3_8",
    "_1_4_3_8/7",
    "_1_4_3_8/1",
    "_1_4_3_8/2",
    "_1_4_3_8/3",
    "_1_4_3_8/4",
    "_1_4_3_8/5",
    "_1_4_3_8/6",

    "_1_4_3_9",
    "_1_4_3_9/7",
    "_1_4_3_9/1",
    "_1_4_3_9/2",
    "_1_4_3_9/3",
    "_1_4_3_9/4",
    "_1_4_3_9/5",
    "_1_4_3_9/6",

    # 3- soil_health:
    "_2_9_1_1",

    # 4_animal_health:
    "_2_10_1_1",

    "_2_10_1_2",
    "_3_3_3_4",

    # 5_biodiversity:
    "_3_3_1_2_1",
    "_3_3_1_2_2",
    "_3_3_1_2_3",
    "_3_3_1_2_4",
    "_3_3_1_2_6",
    "_3_3_1_2_7",
    "_3_3_1_2_8",
    "_3_3_1_6",

    "_3_4_3_3_1",
    "_3_4_3_1_1_2",
    "_3_4_3_4_2",

    # 6- synergy:
    "_3_3_3_1_calculate_2",

    "_2_9_1_1",
    "_2_9_1_1/1",
    "_2_9_1_1/2",
    "_2_9_1_1/3",
    "_2_9_1_1/4",
    "_2_9_1_1/5",
    "_2_9_1_1/6",
    "_2_9_1_1/7",
    "_2_9_1_1/8",
    "_2_9_1_1/9",
    "_2_9_1_1/10",
    "_2_9_1_1/other",

    "_3_3_1_7",
    "_3_3_1_7/cultural_control",
    "_3_3_1_7/repelling_plants",
    "_3_3_1_7/cover_crops",
    "_3_3_1_7/biological-control",
    "_3_3_1_7/spatial_diversity",
    "_3_3_1_7/resistant_varieties",
    "_3_3_1_7/other",

    "_3_3_3_3",
    "_3_3_3_3/Fodder_shrubs",
    "_3_3_3_3/Pasture_rehabilitation",
    "_3_3_3_3/No_manure_management",
    "_3_3_3_3/Exclosures",
    "_3_3_3_3/Pasture_fertilization",
    "_3_3_3_3/Biogas_production",
    "_3_3_3_3/Silvopastoralism",
    "_3_3_3_3/Overgrazing",
    "_3_3_3_3/Producing_feed_legumes",
    "_3_3_3_3/Reducing_grazing_presure",
    "_3_3_3_3/Keeping_improve_breeds",
    "_3_3_3_3/Manure_collection",
    "_3_3_3_3/Improve_manure_storage",
    "_3_3_3_3/Composting",
    "_3_3_3_3/Inclosures",
    "_3_3_3_3/other",

    "_3_3_3_4",
    "_3_3_3_4/1",
    "_3_3_3_4/2",
    "_3_3_3_4/3",
    "_3_3_3_4/4",
    "_3_3_3_4/5",
    "_3_3_3_4/6",
    "_3_3_3_4/7",
    "_3_3_3_4/8",
    "_3_3_3_4/other",

    "_2_12_1",
    "_2_12_1/1",
    "_2_12_1/2",
    "_2_12_1/3",
    "_2_12_1/4",
    "_2_12_1/5",
    "_2_12_1/6",
    "_2_12_1/other__please_specify",

    # 7- economic_diversification:
    "_2_4_1",

    # 8. Knowledge
    "_2_1_1_1",
    "_2_1_1_2",
    "_2_1_1_3",
    "_2_1_1_4",
    "_2_1_1_5",
    "_2_1_1_6",
    "_2_1_1_7",

    # 9_social_values:
    "_2_5_1_1",
    "_2_5_1_2",
    "_2_5_1_3",
    "_2_5_1_4",

    # 10_fairness:
    "_2_6_1_4_1",
    "_2_6_1_4_2",
    "_2_6_1_4_3",
    "_2_6_1_4_4",
    "_2_6_1_4_5",
    "_2_6_1_4_6",

    # 11_connectivity:
    "_2_7_1_1",
    "_2_7_1_1/direct_to_consumer",
    "_2_7_1_1/trader_or_supermarket",
    "_2_7_1_1/middle_man_aggregator",
    "_2_7_1_1/cooperative",
    "_2_7_1_1/other",

    "_2_7_1_2",
    "_2_7_1_2/direct_to_consumer",
    "_2_7_1_2/trader_or_supermarket",
    "_2_7_1_2/middle_man_aggregator",
    "_2_7_1_2/cooperative",
    "_2_7_1_2/other",

    "_2_7_1_3",
    "_2_7_1_3/direct_to_consumer",
    "_2_7_1_3/trader_or_supermarket",
    "_2_7_1_3/middle_man_aggregator",
    "_2_7_1_3/cooperative",
    "_2_7_1_3/other",

    "_2_7_1_4",
    "_2_7_1_4/direct_to_consumer",
    "_2_7_1_4/trader_or_supermarket",
    "_2_7_1_4/middle_man_aggregator",
    "_2_7_1_4/cooperative",
    "_2_7_1_4/other",

    "_2_7_1_5",
    "_2_7_1_5/direct_to_consumer",
    "_2_7_1_5/trader_or_supermarket",
    "_2_7_1_5/middle_man_aggregator",
    "_2_7_1_5/cooperative",
    "_2_7_1_5/other",

    "_2_7_1_6",
    "_2_7_1_6/direct_to_consumer",
    "_2_7_1_6/trader_or_supermarket",
    "_2_7_1_6/middle_man_aggregator",
    "_2_7_1_6/cooperative",
    "_2_7_1_6/other",

    "_1_4_2_2_3",
    "_1_4_2_3_4",
    "_1_4_2_4_3",
    "_1_4_2_5_5",
    "_1_4_2_6_3",
    "_1_4_2_7_4",

    # 12_governance:
    "_2_2_1_1",
    "_2_2_1_2",
    "_2_2_1_3",

    # 13_participation:
    "_2_3_1_4"
  )

  ## check if required original vars exist. If they do not, create them set to null

  missing_cols <- setdiff(required_original_vars, names(bfa_household_data))
  if (length(missing_cols) > 0) {
    bfa_household_data[missing_cols] <- NA
  }

  bfa_household_data2 <- bfa_household_data %>%
    mutate(
      ## Context Variables
      kobo_submission_id = `_id`,
      latitude = `__1_3_latitude`,
      longitude = `__1_3_longitude`,
      sector = `_1_2_1_1`,
      commune = `_1_2_1_2`,
      gender = `_1_2_1_9`,
      birth_year = `_1_2_1_8`,
      age = 2024 - as.numeric(birth_year), ## Age at time of survey, approx;

      land_owned = as.numeric(`_1_4_1_1_1`),
      land_leased = as.numeric(`_1_4_1_1_2`),
      land_use_rights = as.numeric(`_1_4_1_1_3`),

      farm_size = land_owned + land_leased + land_use_rights,

      ### 1_recycling
      seed_source = as.numeric(`_2_8_1_1`),
      organic_fert_source = as.numeric(`_2_8_2_1`),
      livestock_source = as.numeric(`_2_8_3_1`),
      spawn_source = as.numeric(`_2_8_5_1`),
      energy_source = as.numeric(`_2_8_4_5`),

      ### 2_input_reduction
      dry_feed = as.numeric(`_3_4_4_2`),

      sf_methods_1 = `_1_4_3_1/1`,
      sf_methods_2 = `_1_4_3_1/2`,
      sf_methods_3 = `_1_4_3_1/3`,
      sf_methods_0 = `_1_4_3_1/0`,

      pest_methods_1 = `_1_4_3_5/1`,
      pest_methods_2 = `_1_4_3_5/2`,
      pest_methods_3 = `_1_4_3_5/3`,
      pest_methods_0 = `_1_4_3_5/0`,

      fish_feed_type_1 = `_2_8_5_3/1`,
      fish_feed_type_2 = `_2_8_5_3/2`,
      fish_feed_type_3 = `_2_8_5_3/3`,
      fish_feed_type_0 = `_2_8_5_3/0`,

      disease_management_0 = `_1_4_3_8/7`,
      disease_management_1 = `_1_4_3_8/1`,
      disease_management_2 = `_1_4_3_8/2`,
      disease_management_3 = `_1_4_3_8/3`,
      disease_management_4 = `_1_4_3_8/4`,
      disease_management_5 = `_1_4_3_8/5`,
      disease_management_6 = `_1_4_3_8/6`,

      fish_disease_management_0 = `_1_4_3_9/7`,
      fish_disease_management_1 = `_1_4_3_9/1`,
      fish_disease_management_2 = `_1_4_3_9/2`,
      fish_disease_management_3 = `_1_4_3_9/3`,
      fish_disease_management_4 = `_1_4_3_9/4`,
      fish_disease_management_5 = `_1_4_3_9/5`,
      fish_disease_management_6 = `_1_4_3_9/6`,

      ### 3_soil_health

      sf_practices_count = `_2_9_1_1`,

      ### 4_animal_health:
      animal_health = as.numeric(`_2_10_1_1`),

      animal_health_management_count = `_2_10_1_2`,
      fish_land_practice_count = `_3_3_3_4`,

      ### 5_biodiversity:
      bushland_diversity = `_3_3_1_2_1`,
      fallow_land_diversity = `_3_3_1_2_2`,
      hedgerows_diversity = `_3_3_1_2_3`,
      grassland_diversity = `_3_3_1_2_4`,
      forest_patches_diversity = `_3_3_1_2_6`,
      wetlands_diversity = `_3_3_1_2_7`,
      woodlots_diversity = `_3_3_1_2_8`,
      tree_diversity = `_3_3_1_6`,

      livestock_count = as.numeric(`_3_4_3_3_1/Cattle`) +
        as.numeric(`_3_4_3_3_1/Sheep`) +
        as.numeric(`_3_4_3_3_1/Goats`) +
        as.numeric(`_3_4_3_3_1/Horses`) +
        as.numeric(`_3_4_3_3_1/Donkeys`) +
        as.numeric(`_3_4_3_3_1/Mules`) +
        as.numeric(`_3_4_3_3_1/Camels`) +
        as.numeric(`_3_4_3_3_1/Chickens`) +
        as.numeric(`_3_4_3_3_1/Ducks`) +
        as.numeric(`_3_4_3_3_1/Beehives`) +
        as.numeric(`_3_4_3_3_1/other`),
      ## In the BFA data, there are 2 cases of "other" and only a single "other" animal was listed, so we can skip counting the number of other animals.

      crops_count = as.numeric(`_3_4_3_1_1`), ## Was originally listed as: `_3_4_3_1_1_2`, but that is a text inside a repeat for the name of crop grown
      fish_count = as.numeric(`_3_4_3_4_2`), ## Skipped in the BFA data; no farms farmed fish.

      ### 6- synergy:
      ## TODO: get from _3_3_3_2_begin_repeat worksheet
      #  = `_3_3_3_1_calculate_2`,

      # count number of practices selected
      sf_practices_count = as.numeric(`_2_9_1_1/1`) +
        as.numeric(`_2_9_1_1/2`) +
        as.numeric(`_2_9_1_1/3`) +
        as.numeric(`_2_9_1_1/4`) +
        as.numeric(`_2_9_1_1/5`) +
        as.numeric(`_2_9_1_1/6`) +
        as.numeric(`_2_9_1_1/7`) +
        as.numeric(`_2_9_1_1/8`) +
        as.numeric(`_2_9_1_1/9`) +
        as.numeric(`_2_9_1_1/10`) +
        as.numeric(`_2_9_1_1/other`),
      pd_practices_count = as.numeric(`_3_3_1_7/cultural_control`) +
        as.numeric(`_3_3_1_7/repelling_plants`) +
        as.numeric(`_3_3_1_7/cover_crops`) +
        as.numeric(`_3_3_1_7/biological-control`) +
        as.numeric(`_3_3_1_7/spatial_diversity`) +
        as.numeric(`_3_3_1_7/resistant_varieties`) +
        as.numeric(`_3_3_1_7/other`),
      grazing_practice_count = as.numeric(`_3_3_3_3/Fodder_shrubs`) +
        as.numeric(`_3_3_3_3/Pasture_rehabilitation`) +
        as.numeric(`_3_3_3_3/No_manure_management`) +
        as.numeric(`_3_3_3_3/Exclosures`) +
        as.numeric(`_3_3_3_3/Pasture_fertilization`) +
        as.numeric(`_3_3_3_3/Biogas_production`) +
        as.numeric(`_3_3_3_3/Silvopastoralism`) +
        as.numeric(`_3_3_3_3/Overgrazing`) +
        as.numeric(`_3_3_3_3/Producing_feed_legumes`) +
        as.numeric(`_3_3_3_3/Reducing_grazing_presure`) +
        as.numeric(`_3_3_3_3/Keeping_improve_breeds`) +
        as.numeric(`_3_3_3_3/Manure_collection`) +
        as.numeric(`_3_3_3_3/Improve_manure_storage`) +
        as.numeric(`_3_3_3_3/Composting`) +
        as.numeric(`_3_3_3_3/Inclosures`) +
        as.numeric(`_3_3_3_3/other`),
      fish_land_practice_count = as.numeric(`_3_3_3_4/1`) +
        as.numeric(`_3_3_3_4/2`) +
        as.numeric(`_3_3_3_4/3`) +
        as.numeric(`_3_3_3_4/4`) +
        as.numeric(`_3_3_3_4/5`) +
        as.numeric(`_3_3_3_4/6`) +
        as.numeric(`_3_3_3_4/7`) +
        as.numeric(`_3_3_3_4/8`) +
        as.numeric(`_3_3_3_4/other`),
      relationship_actions_count = as.numeric(`_2_12_1/1`) +
        as.numeric(`_2_12_1/2`) +
        as.numeric(`_2_12_1/3`) +
        as.numeric(`_2_12_1/4`) +
        as.numeric(`_2_12_1/5`) +
        as.numeric(`_2_12_1/6`) +
        as.numeric(`_2_12_1/other__please_specify`),

      ### 7- economic_diversification:
      income_count = as.numeric(`_2_4_1/crop`) +
        as.numeric(`_2_4_1/livestock`) +
        as.numeric(`_2_4_1/fish`) +
        as.numeric(`_2_4_1/other_business`) +
        as.numeric(`_2_4_1/casual_labour`) +
        as.numeric(`_2_4_1/formal_labour`) +
        as.numeric(`_2_4_1/transfers`) +
        as.numeric(`_2_4_1/leasing`) +
        as.numeric(`_2_4_1/subsidy`) +
        as.numeric(`_2_4_1/other`),

      ### 8. Knowledge
      ### Integers; often very high (some maxed out "other farmers" = 365...)
      share_extension_workers = `_2_1_1_1`,
      share_consumers = `_2_1_1_2`,
      share_traders = `_2_1_1_3`,
      share_govt = `_2_1_1_4`,
      share_ngos = `_2_1_1_5`,
      share_farmers = `_2_1_1_6`,
      share_researchers = `_2_1_1_7`,

      ### 9_social_values:
      access_healthy_food = as.numeric(`_2_5_1_1`),
      access_diverse_food = as.numeric(`_2_5_1_2`),
      access_seasonal_food = as.numeric(`_2_5_1_3`),
      access_traditional_food = as.numeric(`_2_5_1_4`),

      ### 10_fairness:
      crop_fair_price = as.numeric(`_2_6_1_4_1`),
      livestock_fair_price = as.numeric(`_2_6_1_4_2`),
      fish_fair_price = as.numeric(`_2_6_1_4_3`),
      trees_fair_price = as.numeric(`_2_6_1_4_4`),
      honey_fair_price = as.numeric(`_2_6_1_4_5`),
      other_fair_price = as.numeric(`_2_6_1_4_6`),

      ### 11_connectivity:
      crop_sell_to = `_2_7_1_1`,
      crop_sell_to_direct_to_consumer = `_2_7_1_1/direct_to_consumer`,
      crop_sell_to_trader_or_supermarket = `_2_7_1_1/trader_or_supermarket`,
      crop_sell_to_middle_man_aggregator = `_2_7_1_1/middle_man_aggregator`,
      crop_sell_to_cooperative = `_2_7_1_1/cooperative`,
      crop_sell_to_other = `_2_7_1_1/other`,

      livestock_sell_to = `_2_7_1_2`,
      livestock_sell_to_direct_to_consumer = `_2_7_1_2/direct_to_consumer`,
      livestock_sell_to_trader_or_supermarket = `_2_7_1_2/trader_or_supermarket`,
      livestock_sell_to_middle_man_aggregator = `_2_7_1_2/middle_man_aggregator`,
      livestock_sell_to_cooperative = `_2_7_1_2/cooperative`,
      livestock_sell_to_other = `_2_7_1_2/other`,

      fish_sell_to = `_2_7_1_3`,
      fish_sell_to_direct_to_consumer = `_2_7_1_3/direct_to_consumer`,
      fish_sell_to_trader_or_supermarket = `_2_7_1_3/trader_or_supermarket`,
      fish_sell_to_middle_man_aggregator = `_2_7_1_3/middle_man_aggregator`,
      fish_sell_to_cooperative = `_2_7_1_3/cooperative`,
      fish_sell_to_other = `_2_7_1_3/other`,

      trees_sell_to = `_2_7_1_4`,
      trees_sell_to_direct_to_consumer = `_2_7_1_4/direct_to_consumer`,
      trees_sell_to_trader_or_supermarket = `_2_7_1_4/trader_or_supermarket`,
      trees_sell_to_middle_man_aggregator = `_2_7_1_4/middle_man_aggregator`,
      trees_sell_to_cooperative = `_2_7_1_4/cooperative`,
      trees_sell_to_other = `_2_7_1_4/other`,

      honey_sell_to = `_2_7_1_5`,
      honey_sell_to_direct_to_consumer = `_2_7_1_5/direct_to_consumer`,
      honey_sell_to_trader_or_supermarket = `_2_7_1_5/trader_or_supermarket`,
      honey_sell_to_middle_man_aggregator = `_2_7_1_5/middle_man_aggregator`,
      honey_sell_to_cooperative = `_2_7_1_5/cooperative`,
      honey_sell_to_other = `_2_7_1_5/other`,

      other_sell_to = `_2_7_1_6`,
      other_sell_to_direct_to_consumer = `_2_7_1_6/direct_to_consumer`,
      other_sell_to_trader_or_supermarket = `_2_7_1_6/trader_or_supermarket`,
      other_sell_to_middle_man_aggregator = `_2_7_1_6/middle_man_aggregator`,
      other_sell_to_cooperative = `_2_7_1_6/cooperative`,
      other_sell_to_other = `_2_7_1_6/other`,

      # "sales" % values - they appear to not be used in the calculations of AE indicator scores, so commenting them out for now.
      # = `_1_4_2_2_3`,
      # = `_1_4_2_3_4`,
      # = `_1_4_2_4_3`,
      # = `_1_4_2_5_5`,
      # = `_1_4_2_6_3`,
      # = `_1_4_2_7_4`,

      ### 12_governance:
      activities_land_management = `_2_2_1_1`,
      influence_land_management = `_2_2_1_2`,
      land_management_view = `_2_2_1_3`,

      ### 13_participation:
      association_effectiveness = `_2_3_1_4`
    ) %>%
    select(
      all_of(required_vars)
    )
}

library(tidyverse)
library(jsonlite)
library(httr)
library(main_surveys.table)

################################################################################
# IMPORT main_surveys FROM main_surveysBASE
################################################################################ 

source("data_processing//get_db_connection.R")

agroecology_scores <- main_surveys%>%select(id)

################################################################################
# RECYCLING
################################################################################


recylcing_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      recycling_1_score = seed_source,
      recyling_1_label = factor(
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
      recycling_2_score = organic_fert_source,
      recyling_2_label = factor(
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
      recycling_3_score = livestock_source,
      recyling_3_label = factor(
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
      recycling_4_score = spawn_source,
      recyling_4_label = factor(
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
      recycling_5_score = energy_source,
      recyling_5_label = factor(
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
    )%>%
    select(id, starts_with("recycling"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
#  INPUT REDUCTION
################################################################################

input_reduction_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      input_reduction_1_score = case_when(
          sf_methods == 3 ~ 5,
          sf_methods == 2 |
            (sf_methods.2 == 1 & sf_methods.3 == 1 & sf_methods.1 == 0) ~ 4,
          (sf_methods.2 == 1 & sf_methods.3 == 1 & sf_methods.1 == 1) |
            (sf_methods.2 == 0 & sf_methods.3 == 1 & sf_methods.1 == 1) ~ 3,
          sf_methods.2 == 1 & sf_methods.3 == 0 & sf_methods.1 == 1 ~ 2,
          sf_methods == 1 ~ 1,
          sf_methods == 0 ~ 5
      ),
      input_reduction_1_label = case_when(
        sf_methods == 0 ~ "No ecological practices, chemical or organic fertilizer were applied",
        sf_methods == 3 ~ "Only ecological practices are applied",
        input_reduction_1_score == 4 ~ "Combination of ecological practices and organic fertilizers or manure are applied/ Only organic fertilizers or manure are applied",
        input_reduction_1_score == 3 ~ "Combination of ecological practices and organic fertilizers or manure and/or chemical fertilizers is applied",
        input_reduction_1_score == 2 ~ "Combination of chemical fertilizers and organic fertilizers or manure are applied",
        input_reduction_1_score == 1 ~ "Only chemical fertilizers applied"
      ),
      input_reduction_2_score = case_when(
        pest_methods == 3 ~ 5,
        pest_methods == 2 |
          (pest_methods.2 == 1 & pest_methods.3 == 1 & pest_methods.1 == 0) ~ 4,
        (pest_methods.2 == 1 & pest_methods.3 == 1 & pest_methods.1 == 1) |
          (pest_methods.2 == 0 & pest_methods.3 == 1 & pest_methods.1 == 1) ~ 3,
        pest_methods.2 == 1 & pest_methods.3 == 0 & pest_methods.1 == 1 ~ 2,
        pest_methods == 1 ~ 1,
        pest_methods == 0 ~ 5
      ),
      input_reduction_2_label = case_when(
        sf_methods == 0 ~ "No ecological practices, chemical or non-chemical pestices were applied",
        sf_methods == 3 ~ "Only ecological practices are applied",
        input_reduction_2_score == 4 ~ "Combination of ecological practices and non-chemical fungicides/pesticides/herbicides /  Only non-chemical fungicides/pesticides/herbicides area applied",
        input_reduction_2_score == 3 ~ "Combination of chemical and non-chemical fungicides/pesticides/herbicides and/or ecological practices",
        input_reduction_2_score == 2 ~ "Combination of chemical and non-chemical fungicides/pesticides/herbicides",
        input_reduction_2_score == 1 ~ "Only chemical fungicides/pesticides/herbicides are applied"
      ),
      input_reduction_3_score = dry_feed,
      inptu_reduction_3_label = factor(
        dry_feed,
        levels = c(1:5),
        labels = c("All the time",
                   "Often",
                   "Sometimes",
                   "Rarely",
                   "Never")
      ),
      input_reduction_4_score = case_when(
        fish_feed_type == 1 ~ 5,
        fish_feed_type == 2 |
          (fish_feed_type.1 == 1 & fish_feed_type.2 == 1 & fish_feed_type.3 == 0) ~ 4,
        (fish_feed_type.1 == 1 & fish_feed_type.2 == 1 & fish_feed_type.3 == 1) ~ 3,
        (fish_feed_type.1 == 0 & fish_feed_type.2 == 1 & fish_feed_type.3 == 1) |
          (fish_feed_type.1 == 1 & fish_feed_type.2 == 0 & fish_feed_type.3 == 1)~ 2, #CHECK WITH HOLPA ON THE COMBINATION OF NATURAL AND CHEMICAL
        fish_feed_type ==3 ~ 1
      ),
      input_reduction_4_label = case_when(
        input_reduction_4_score == 5 ~ "Only natural feeds used",
        input_reduction_4_score == 4 ~ "Combination of natural and prepared organic feed used. Or only organic",
        input_reduction_4_score == 3 ~ "Combination of natural, preprepared organic feeds and prepared chemical feeds used",
        input_reduction_4_score == 2 ~ "Combination of prepared organic and chemical feeds used",
        input_reduction_4_score == 1 ~ "Only chemical feeds used"
      ),
      input_reduction_5_score = case_when(
        # =0
         disease_management == 0 ~ 5,
         # 5 or 6
         (disease_management.5 == 1 | disease_management.6 == 1) &
           (disease_management.1 == 0 & disease_management.2 == 0 & 
              disease_management.3 == 0 & disease_management.4 == 0) ~ 5,
        #  3 or 4 & 5 or 6
         ((disease_management.1 == 0 & disease_management.2 == 0) &
           ((disease_management.3 == 1 | disease_management.4 == 1) &
              (disease_management.5 == 1 | disease_management.6 == 1))) ~ 4,
         # 3 or 4
         (disease_management.3 == 1 | disease_management.4 == 1) &
           (disease_management.1 == 0 & disease_management.2 == 0 & 
              disease_management.5 == 0 & disease_management.6 == 0) ~ 4,
        #  1 or 2 & 5 or 6
         ((disease_management.3 == 0 & disease_management.4 == 0) &
            ((disease_management.1 == 1 | disease_management.2 == 1) &
               (disease_management.5 == 1 | disease_management.6 == 1))) ~ 3,
        # 1 or 2 & 3 or 4 & 5 or 6
        (disease_management.5 == 1 | disease_management.6 == 1) &
          (disease_management.3 == 1 | disease_management.4 == 1) &
            (disease_management.1 == 1 | disease_management.2 == 1) ~ 3,
        # 1 or 2 & 3 or 4
        disease_management.5 == 0 & disease_management.6 == 0 & 
          ((disease_management.1 == 1 | disease_management.2 == 1) &
             (disease_management.3 == 1 | disease_management.4 == 1)) ~ 2,
        #  1 or 2
         (disease_management.1 == 1 | disease_management.2 == 1) &
           (disease_management.5 == 0 & disease_management.6 == 0 &
              disease_management.3 == 0 & disease_management.4 == 0) ~ 1
      ),
      input_reduction_5_label = case_when(
        disease_management == 0 ~ "No action taken",

        (disease_management.5 == 1 | disease_management.6 == 1) &
          (disease_management.1 == 0 & disease_management.2 == 0 &
             disease_management.3 == 0 & disease_management.4 == 0) ~ "Only ecological practices/treatments",

        input_reduction_5_score == 4 ~ "Combination of ecological practices/treatments and organic inputs.OR only organic inputs",
        input_reduction_5_score == 3 ~ "Combination of ecological practices/treatments, and chemical and/or organic inputs",
        input_reduction_5_score == 2 ~ "Combination of chemical and organic inputs",
        input_reduction_5_score == 1 ~ "Only chemical inputs"
      ),
      input_reduction_6_score = case_when(
        # =0
        fish_disease_management == 0 ~ 5,
        # 5 or 6
        (fish_disease_management.5 == 1 | fish_disease_management.6 == 1) &
          (fish_disease_management.1 == 0 & fish_disease_management.2 == 0 & 
             fish_disease_management.3 == 0 & fish_disease_management.4 == 0) ~ 5,
        #  3 or 4 & 5 or 6
        ((fish_disease_management.1 == 0 & fish_disease_management.2 == 0) &
           ((fish_disease_management.3 == 1 | fish_disease_management.4 == 1) &
              (fish_disease_management.5 == 1 | fish_disease_management.6 == 1))) ~ 4,
        # 3 or 4
        (fish_disease_management.3 == 1 | fish_disease_management.4 == 1) &
          (fish_disease_management.1 == 0 & fish_disease_management.2 == 0 & 
             fish_disease_management.5 == 0 & fish_disease_management.6 == 0) ~ 4,
        #  1 or 2 & 5 or 6
        ((fish_disease_management.3 == 0 & fish_disease_management.4 == 0) &
           ((fish_disease_management.1 == 1 | fish_disease_management.2 == 1) &
              (fish_disease_management.5 == 1 | fish_disease_management.6 == 1))) ~ 3,
        # 1 or 2 & 3 or 4 & 5 or 6
        (fish_disease_management.5 == 1 | fish_disease_management.6 == 1) &
          (fish_disease_management.3 == 1 | fish_disease_management.4 == 1) &
          (fish_disease_management.1 == 1 | fish_disease_management.2 == 1) ~ 3,
        # 1 or 2 & 3 or 4
        fish_disease_management.5 == 0 & fish_disease_management.6 == 0 & 
          ((fish_disease_management.1 == 1 | fish_disease_management.2 == 1) &
             (fish_disease_management.3 == 1 | fish_disease_management.4 == 1)) ~ 2,
        #  1 or 2
        (fish_disease_management.1 == 1 | fish_disease_management.2 == 1) &
          (fish_disease_management.5 == 0 & fish_disease_management.6 == 0 &
             fish_disease_management.3 == 0 & fish_disease_management.4 == 0) ~ 1
      ),
      input_reduction_6_label = case_when(
        fish_disease_management == 0 ~ "No action taken",
        
        (fish_disease_management.5 == 1 | fish_disease_management.6 == 1) &
          (fish_disease_management.1 == 0 & fish_disease_management.2 == 0 &
             fish_disease_management.3 == 0 & fish_disease_management.4 == 0) ~ "Only ecological practices/treatments",
        
        input_reduction_6_score == 4 ~ "Combination of ecological practices/treatments and organic inputs.OR only organic inputs",
        input_reduction_6_score == 3 ~ "Combination of ecological practices/treatments, and chemical and/or organic inputs",
        input_reduction_6_score == 2 ~ "Combination of chemical and organic inputs",
        input_reduction_6_score == 1 ~ "Only chemical inputs"
      )
    )%>%
     select(id, starts_with("input_reduction"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
  
}

################################################################################
# SOIL HEALTH
################################################################################

soil_health_score <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      soil_health_score = case_when(
        sf_practices_count >= 4 ~ 5,
        sf_practices_count == 3 ~ 4,
        sf_practices_count == 2 ~ 3,
        sf_practices_count == 1 ~ 2,
        sf_practices_count == 0 ~ 1,
      ),
      soil_health_label = case_when(
        soil_health_score == 5 ~ "Implementing 4 or more practices",
        soil_health_score == 4 ~ "Implementing 3 practices",
        soil_health_score == 3 ~ "Implementing 2 practices",
        soil_health_score == 2 ~ "Implementing 1 practice",
        soil_health_score == 1 ~ "Not implementing any practice"
      )
    )%>%
    select(id, soil_health_score, soil_health_label)
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)

}

################################################################################
# ANIMAL HEALTH
################################################################################

animal_health_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      animal_health_1_score = animal_health,
      animal_health_1_label = factor(
        seed_source,
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
        animal_health_management_count >= 4 ~ 5,
        animal_health_management_count == 3 ~ 4,
        animal_health_management_count == 2 ~ 3,
        animal_health_management_count == 1 ~ 2,
        animal_health_management_count == 0 ~ 1,
      ),
      animal_health_2_label = case_when(
        animal_health_2_score == 5 ~ "Implementing 4 or more practices",
        animal_health_2_score == 4 ~ "Implementing 3 practices",
        animal_health_2_score == 3 ~ "Implementing 2 practices",
        animal_health_2_score == 2 ~ "Implementing 1 practice",
        animal_health_2_score == 1 ~ "Not implementing any practice"
      ),
      animal_health_3_score = case_when(
        fish_land_practice_count >= 4 ~ 5,
        fish_land_practice_count == 3 ~ 4,
        fish_land_practice_count == 2 ~ 3,
        fish_land_practice_count == 1 ~ 2,
        fish_land_practice_count == 0 ~ 1,
      ),
      animal_health_3_label = case_when(
        animal_health_3_score == 5 ~ "Implementing 4 or more practices",
        animal_health_3_score == 4 ~ "Implementing 3 practices",
        animal_health_3_score == 3 ~ "Implementing 2 practices",
        animal_health_3_score == 2 ~ "Implementing 1 practice",
        animal_health_3_score == 1 ~ "Not implementing any practice"
      )
    )%>%
    select(id, animal_health_1_score:animal_health_3_label)
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
}

################################################################################
# BIODIVERSITY
################################################################################

div_score <- function(var){
 
  scores <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
      {{var}} == "high" ~ 5,
      {{var}} == "medium" ~ 3.67,
      {{var}} == "low" ~ 2.33,
      {{var}} == "none" ~ 1
    )
    )
  
  return(scores[[1]])
   
}

div_labels <- function(var){
  
  labels <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} == "high" ~ "High: five or more species with different heights, woodiness or flowering seasons",
          {{var}} == "medium" ~ "Medium: two to four species",
          {{var}} == "low" ~ "Low: only one species",
          {{var}} == "none" ~ "None"
        )
    )
  
  return(labels[[1]])
}

biodiversity_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      biodiversity_1_score = case_when(
        crop_count <= quantile(crop_count, 0.2, na.rm = TRUE) ~ 1,
        crop_count <= quantile(crop_count, 0.4, na.rm = TRUE) ~ 2,
        crop_count <= quantile(crop_count, 0.6, na.rm = TRUE) ~ 3,
        crop_count <= quantile(crop_count, 0.8, na.rm = TRUE) ~ 4,
        crop_count  > quantile(crop_count, 0.8, na.rm = TRUE) ~ 5
      ), 
      biodiversity_1_label = factor(
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
        livestock_count <= quantile(livestock_count, 0.2, na.rm = TRUE) ~ 1,
        livestock_count <= quantile(livestock_count, 0.4, na.rm = TRUE) ~ 2,
        livestock_count <= quantile(livestock_count, 0.6, na.rm = TRUE) ~ 3,
        livestock_count <= quantile(livestock_count, 0.8, na.rm = TRUE) ~ 4,
        livestock_count  > quantile(livestock_count, 0.8, na.rm = TRUE) ~ 5
      ), 
      biodiversity_2_label = factor(
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
        fish_count <= quantile(fish_count, 0.2, na.rm = TRUE) ~ 1,
        fish_count <= quantile(fish_count, 0.4, na.rm = TRUE) ~ 2,
        fish_count <= quantile(fish_count, 0.6, na.rm = TRUE) ~ 3,
        fish_count <= quantile(fish_count, 0.8, na.rm = TRUE) ~ 4,
        fish_count  > quantile(fish_count, 0.8, na.rm = TRUE) ~ 5
      ), 
      biodiversity_3_label = factor(
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
      biodiversity_4_score = div_score(tree_diversity),
      biodiversity_4_label = div_labels(tree_diversity),
      biodiversity_5_score = div_score(bushland_diversity),
      biodiversity_5_label = div_labels(bushland_diversity),
      biodiversity_6_score = div_score(fallow_land_diversity),
      biodiversity_6_label = div_labels(fallow_land_diversity),
      biodiversity_7_score = div_score(hedgerows_diversity),
      biodiversity_7_label = div_labels(hedgerows_diversity),
      biodiversity_8_score = div_score(grassland_diversity),
      biodiversity_8_label = div_labels(grassland_diversity),
      biodiversity_9_score = div_score(forest_patches_diversity),
      biodiversity_9_label = div_labels(forest_patches_diversity),
      biodiversity_10_score = div_score(wetlands_diversity),
      biodiversity_10_label = div_labels(wetlands_diversity),
      biodiversity_11_score = div_score(woodlots_diversity),
      biodiversity_11_label = div_labels(woodlots_diversity)
    )%>%
    select(id, starts_with("biodiversity"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# SYNERGY
################################################################################

syn_score <- function(var){
  
  scores <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} >= 4 ~ 5,
          {{var}} == 3 ~ 4,
          {{var}} == 2 ~ 3,
          {{var}} == 1 ~ 2,
          {{var}} == 0 ~ 1,
        )
    )
  
  return(scores[[1]])
  
}

syn_labels <- function(var){
  
  labels <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} >= 4 ~ "Implementing 4 or more practices",
          {{var}} == 3 ~ "Implementing 3 practices",
          {{var}} == 2 ~ "Implementing 2 practices",
          {{var}} == 1 ~ "Implementing 1 practice",
          {{var}} == 0 ~ "Not implementing any practice"
        )
    )
  
  return(labels[[1]])
}

synergy_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      synergy_1_score = syn_score(ecological_practices_count), 
      synergy_1_label = syn_labels(ecological_practices_count),
      synergy_2_score = syn_score(sf_practices_count),  
      synergy_2_label = syn_labels(sf_practices_count),
      synergy_3_score = syn_score(pd_practices_count), 
      synergy_3_label = syn_labels(pd_practices_count),
      synergy_4_score = syn_score(grazing_practice_count), 
      synergy_4_label = syn_labels(grazing_practice_count),
      synergy_5_score = syn_score(fish_land_practice_count),  
      synergy_5_label = syn_labels(fish_land_practice_count),
      synergy_6_score = syn_score(relationship_actions_count),
      synergy_6_label = syn_labels(relationship_actions_count)
    )%>%
    select(id, starts_with("synergy_"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
    
}

################################################################################
# ECONOMIC DIVERSIFICATION
################################################################################

economic_div_score <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      enconomic_diversification_score =
        case_when(
          income_count >= 5 ~ 5,
          income_count == 4 ~ 4,
          income_count == 3 ~ 3,
          income_count == 2 ~ 2,
          income_count == 1 ~ 1
        ),
      enconomic_diversification_label =
        case_when(
          income_count >= 5 ~ "Five or more methods of income generation",
          income_count == 4 ~ "Four methods of income generation",
          income_count == 3 ~ "Three methods of income generation",
          income_count == 2 ~ "Two methods of income generation",
          income_count == 1 ~ "One method of income generation"
        )
    )%>%
    select(id, starts_with("economic_diversification"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# CO-CREATION OF KNOWLEDGE
################################################################################

cck_score <- function(var){
  
  scores <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} >= 5 ~ 5,
          {{var}} == 4 ~ 4,
          {{var}} %in% c(2,3) ~ 3,
          {{var}} == 1 ~ 2,
          {{var}} == 0 ~ 1,
        )
    )
  
  return(scores[[1]])
  
}

cck_labels <- function(var){
  
  labels <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} >= 5 ~ "5 or more times per year",
          {{var}} == 4 ~ "4 times per year",
          {{var}} %in% c(2,3) ~ "2 to 3 times per year",
          {{var}} == 1 ~ "1 time per year",
          {{var}} == 0 ~ "Never"
        )
    )
  
  return(labels[[1]])
}

cc_knowledge_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      co_creation_knowledge_1_score = cck_score(share_extension_workers),
      co_creation_knowledge_1_label = cck_labels(share_extension_workers),
      co_creation_knowledge_2_score = cck_score(share_consumers),
      co_creation_knowledge_2_label = cck_labels(share_consumers),
      co_creation_knowledge_3_score = cck_score(share_traders),
      co_creation_knowledge_3_label = cck_labels(share_traders),
      co_creation_knowledge_4_score = cck_score(share_govt),
      co_creation_knowledge_4_label = cck_labels(share_govt),
      co_creation_knowledge_5_score = cck_score(share_ngos),
      co_creation_knowledge_5_label = cck_labels(share_ngos),
      co_creation_knowledge_6_score = cck_score(share_farmers),
      co_creation_knowledge_6_label = cck_labels(share_farmers),
      co_creation_knowledge_7_score = cck_score(share_researchers),
      co_creation_knowledge_7_label = cck_labels(share_researchers)
    )%>%
    select(id, starts_with("co_creation_knowledge"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# SOCIAL VALUES AND DIET
################################################################################

diet_labels <- function(var){
  
  labels <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} == 5 ~ "Good access",
          {{var}} == 4 ~ "Fairly good",
          {{var}} == 3 ~ "Moderate access",
          {{var}} == 2 ~ "Limited access",
          {{var}} == 1 ~ "No access at all"
        )
    )
  
  return(labels[[1]])
}

diet_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      social_values_diet_1_score = access_healthy_food,
      social_values_diet_1_label = diet_labels(access_healthy_food),
      social_values_diet_2_score = access_diverse_food,
      social_values_diet_2_label = diet_labels(access_diverse_food),
      social_values_diet_3_score = access_seasonal_food,
      social_values_diet_3_label = diet_labels(access_seasonal_food),
      social_values_diet_4_score = access_traditional_food,
      social_values_diet_5_label = diet_labels(access_traditional_food)
    )%>%
    select(id, starts_with("social_values_diet"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# FAIRNESS
################################################################################

fairness_labels <- function(var){
  
  labels <- main_surveys%>%
    select({{var}})%>%
    mutate(
      {{var}} := 
        case_when(
          {{var}} == 5 ~ "Always get a fair price",
          {{var}} == 4 ~ "Usually get a fair price, depending on the product",
          {{var}} == 3 ~ "Occasionally get a fair price, depending on the product",
          {{var}} == 2 ~ "Rarely get a fair price",
          {{var}} == 1 ~ "Never get a fair price./I don't know"
        )
    )
  
  return(labels[[1]])
}

fairness_scores <- function(){
  
  other_prods <- products%>%
    group_by(farm_id)%>%
    summarise(fairness_6_score = round(mean(other_prod_fair_price, na.rm = TRUE),0))%>%
    mutate(fariness_6_label = case_when(
      fairness_6_score == 5 ~ "Always get a fair price",
      fairness_6_score == 4 ~ "Usually get a fair price, depending on the product",
      fairness_6_score == 3 ~ "Occasionally get a fair price, depending on the product",
      fairness_6_score == 2 ~ "Rarely get a fair price",
      fairness_6_score == 1 ~ "Never get a fair price./I don't know"
    ))
  
  main_surveys <- main_surveys%>%
    left_join(other_prods, by = c("id" = "farm_id"))%>%
    mutate(
      fairness_1_score = crop_fair_price,
      fariness_1_label = fairness_labels(crop_fair_price),
      fairness_2_score = livestock_fair_price,
      fariness_2_label = fairness_labels(livestock_fair_price),
      fairness_3_score = fish_fair_price,
      fariness_3_label = fairness_labels(fish_fair_price),
      fairness_4_score = trees_fair_price,
      fariness_4_label = fairness_labels(trees_fair_price),
      fairness_5_score = honey_fair_price,
      fariness_5_label = fairness_labels(honey_fair_price)
    )%>%
    select(id, starts_with("fairness_"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
}

################################################################################
# CONNECTIVITY
################################################################################

#CHECK DEFINITION FOR OTHERS

connectivity_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      connectivity_1_score = case_when(
        crop_buyer.direct_to_consumer == 1 ~ 5,
        crop_buyer.trader_or_supermarket == 1 | crop_buyer.cooperative == 1 ~ 4,
        crop_buyer.retailers == 1 ~ 3,
        crop_buyer.middle_man_aggregator == 1 ~ 2,
        crop_sales == 0 ~ 1,
        crop_buyer.other == 1 ~ 4
      ),
      connectivity_1_label = case_when(
        connectivity_1_score == 5 ~ "Directly to consumers",
        connectivity_1_score == 4 ~ "To farmers organistion/cooperative",
        connectivity_1_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_1_score == 2 ~ "To a middle man/aggregator",
        connectivity_1_score == 1 ~ "Does not sell its products"
      ),
      connectivity_2_score = case_when(
        livestock_buyer.direct_to_consumer == 1 ~ 5,
        livestock_buyer.trader_or_supermarket == 1 | livestock_buyer.cooperative == 1 ~ 4,
        livestock_buyer.retailers == 1 ~ 3,
        livestock_buyer.middle_man_aggregator == 1 ~ 2,
        livestock_sales == 0 ~ 1,
        livestock_buyer.other == 1 ~ 4
      ),
      connectivity_2_label = case_when(
        connectivity_2_score == 5 ~ "Directly to consumers",
        connectivity_2_score == 4 ~ "To farmers organistion/cooperative",
        connectivity_2_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_2_score == 2 ~ "To a middle man/aggregator",
        connectivity_2_score == 1 ~ "Does not sell its products"
      ),
      connectivity_3_score = case_when(
        fish_buyer.direct_to_consumer == 1 ~ 5,
        fish_buyer.trader_or_supermarket == 1 | fish_buyer.cooperative == 1 ~ 4,
        fish_buyer.retailers == 1 ~ 3,
        fish_buyer.middle_man_aggregator == 1 ~ 2,
        fish_sales == 0 ~ 1,
        fish_buyer.other == 1 ~ 4
      ),
      connectivity_3_label = case_when(
        connectivity_3_score == 5 ~ "Directly to consumers",
        connectivity_3_score == 4 ~ "To farmers organistion/cooperative",
        connectivity_3_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_3_score == 2 ~ "To a middle man/aggregator",
        connectivity_3_score == 1 ~ "Does not sell its products"
      ),
      connectivity_4_score = case_when(
        trees_buyer.direct_to_consumer == 1 ~ 5,
        trees_buyer.trader_or_supermarket == 1 | trees_buyer.cooperative == 1 ~ 4,
        trees_buyer.retailers == 1 ~ 3,
        trees_buyer.middle_man_aggregator == 1 ~ 2,
        trees_sales == 0 ~ 1,
        trees_buyer.other == 1 ~ 4
      ),
      connectivity_4_label = case_when(
        connectivity_4_score == 5 ~ "Directly to consumers",
        connectivity_4_score == 4 ~ "To farmers organistion/cooperative",
        connectivity_4_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_4_score == 2 ~ "To a middle man/aggregator",
        connectivity_4_score == 1 ~ "Does not sell its products"
      ),
      connectivity_5_score = case_when(
        honey_buyer.direct_to_consumer == 1 ~ 5,
        honey_buyer.trader_or_supermarket == 1 | honey_buyer.cooperative == 1 ~ 4,
        honey_buyer.retailers == 1 ~ 3,
        honey_buyer.middle_man_aggregator == 1 ~ 2,
        honey_sales == 0 ~ 1,
        honey_buyer.other == 1 ~ 4
      ),
      connectivity_5_label = case_when(
        connectivity_5_score == 5 ~ "Directly to consumers",
        connectivity_5_score == 4 ~ "To farmers organistion/cooperative",
        connectivity_5_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_5_score == 2 ~ "To a middle man/aggregator",
        connectivity_5_score == 1 ~ "Does not sell its products"
      ),
      connectivity_6_score = case_when(
        other_prod_buyer.direct_to_consumer == 1 ~ 5,
        other_prod_buyer.trader_or_supermarket == 1 | other_prod_buyer.cooperative == 1 ~ 4,
        other_prod_buyer.retailers == 1 ~ 3,
        other_prod_buyer.middle_man_aggregator == 1 ~ 2,
        other_prod_sales == 0 ~ 1,
        other_prod_buyer.other == 1 ~ 4
      ),
      connectivity_6_label = case_when(
        connectivity_6_score == 5 ~ "Directly to consumers",
        connectivity_6_score == 4 ~ "To farmers organistion/cooperative",
        connectivity_6_score == 3 ~ "To retailers such us supermarkets, grocery stores, or restaurants.",
        connectivity_6_score == 2 ~ "To a middle man/aggregator",
        connectivity_6_score == 1 ~ "Does not sell its products"
      )
    )%>%
    select(id, starts_with("connectivity"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# LAND AND NATURAL RESOURCE GOVERNANCE
################################################################################

governance_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      governance_1_score = activities_land_management,
      governance_1_label = factor(
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
      governance_2_score = influence_land_management,
      governance_2_label = factor(
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
      governance_3_score = land_management_view,
      governance_3_label = factor(
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
    )%>%
    select(id, starts_with("governance_"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# PARTICIPATION
################################################################################

participation_scores <- function(){
  
  main_surveys <- main_surveys%>%
    mutate(
      participation_score = ifelse(
        association_effectiveness == 999, 1, association_effectiveness
      ),
      participation_label = case_when(
        association_effectiveness == 5 ~ "Associations/organizations demonstrate exceptional effectiveness in supporting farmers' business ventures,
        offering comprehensive assistance, fostering growth, and ensuring long-term success",
        association_effectiveness == 4 ~ "Associations/organizations play a significant role in supporting farmers' businesses,
        providing valuable resources, market opportunities, and essential services",
        association_effectiveness == 3 ~ "Associations/organizations offer satisfactory support to farmers,
        aiding them in various aspects of their businesses (e.g., market access, information sharing, and capacity development)",
        association_effectiveness == 2 ~ "Associations/organizations provide limited support to farmers in business,
        with marginal impact on their overall success",
        association_effectiveness == 1 ~ "Associations/organizations offer no support to farmers' businesses.",
        association_effectiveness == 999 ~ "I don't know"
        
      )
    )%>%
    select(id, starts_with("participation_"))
  
  agroecology_scores <- left_join(agroecology_scores,
                                  main_surveys)
  
  return(agroecology_scores)
  
}

################################################################################
# CREATE AGROECOLOGY SCORE main_surveysTABLE
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

################################################################################
# WRITE TABLE TO main_surveysBSE
################################################################################

dbWriteTable(con,"agroecology_scores",agroecology_scores,overwrite=TRUE)

dbDisconnect(con)

library(RMariaDB)
library(ssh)
library(sys)
library(tidyverse)
library(jsonlite)
library(dotenv)
library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))

env_path <- paste(getwd(), ".env", sep = '/')

dotenv::load_dot_env(env_path)

################################################################################
# GET DATA TABLES
################################################################################

get_db <- function() {
  return(dbConnect(RMariaDB::MariaDB(),
                   dbname = Sys.getenv("DB_DATABASE"),
                   host = Sys.getenv("DB_HOST"),
                   port = as.integer(Sys.getenv("DB_PORT")),
                   user = Sys.getenv("DB_USERNAME"),
                   password = Sys.getenv("DB_PASSWORD"),
                   bigint = "numeric",
                   int = "numeric"
                   
  ))
}

con <- get_db()

get_dataset <- function(table){

  data <- dbGetQuery(con,paste("SELECT * FROM ",table))%>%
    select(-properties)
  
  getquery <- paste0("select id, cast(properties as CHAR) from ", table)
  
  booleans <- dbGetQuery(con,getquery)
  colnames(booleans)[2] <- "json"
  booleans <- booleans %>%
  as_tibble() %>%
  filter(!is.na(json))%>%
  mutate(j = purrr::map(json, jsonlite::fromJSON)) %>%
  tidyr::unnest_wider(j)

data <- left_join(data, select(booleans, -json))

return(data)

}

farms <- dbGetQuery(con, "SELECT * FROM farms")
main_surveys <- get_dataset("farm_survey_data")
products <- get_dataset("products") 
permanent_workers <- get_dataset("permanent_workers")
seasonal_workers <- get_dataset("seasonal_worker_seasons")
ecological_practices <- dbGetQuery(con,"SELECT * FROM ecological_practices")
crops <- get_dataset("crops")
livestock <- get_dataset("livestocks")
livestock_uses <- get_dataset("livestock_uses")
fish <- get_dataset("fishes")
fish_uses <- get_dataset("fish_uses")
fieldwork_sites <- get_dataset("fieldwork_sites")

# main_surveys <- main_surveys%>%
#   left_join(farms%>%select(id, "team_id" = team_code))

################################################################################
# REFORMAT POSSIBLE MISSING CODED DATA
################################################################################

na_99 <- function(data){
  
  data <- data%>%
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x))
  
  return(data)
  
}

missing_codes <- c(99,999,9999,99999, 888, 8888, 8888, 555, 5555, 55555, 777, 7777, 77777)

main_surveys <- na_99(main_surveys)
products <- na_99(products)
permanent_workers <- na_99(permanent_workers)
seasonal_workers <- na_99(seasonal_workers)
ecological_practices <- na_99(ecological_practices)
crops <- na_99(crops)
livestock <- na_99(livestock)
livestock_uses <- na_99(livestock_uses)
fish <- na_99(fish)
fish_uses <- na_99(fish_uses)
fieldwork_sites <- na_99(fieldwork_sites)

missing_vars_main <- c(
  "chem_fert_applied",
  "chem_fert_applied_kg",
  "chem_fert_area",
  "chem_fert_area_ha",
  "chem_fert_applied_per_area",
  "chem_fert_kg_ha",
  "own_organic_fert_applied",
  "own_organic_fert_applied_kg",
  "own_organic_fert_area",
  "own_organic_fert_area_ha",
  "own_organic_fert_applied_per_area",
  "own_organic_fert_kg_ha",
  "bought_organic_fert_applied",
  "bought_organic_fert_applied_kg",
  "bought_organic_fert_area",
  "bought_organic_fert_area_ha",
  "bought_organic_fert_applied_per_area",
  "bought_organic_fert_kg_ha",
  "chemical_applied",
  "chemical_applied_kg",
  "chemical_area",
  "chemical_area_ha",
  "chemical_applied_per_area",
  "chemical_kg_ha",
  "non_chemical_applied",
  "non_chemical_applied_kg",
  "non_chemical_area",
  "non_chemical_area_ha",
  "non_chemical_applied_per_area",
  "non_chemical_kg_ha",
  "total_crop_area",
  "total_crop_area_ha",
  "income_crops",
  "income_livestock",
  "income_fish",
  "income_family_business",
  "income_casual_labour",
  "income_formal_labour",
  "income_cash",
  "income_leasing",
  "income_subsidy",
  "income_other",
  "income_sum",
  "livestock_land_own",
  "livestock_land_own_ha",
  "livestock_land_share",
  "livestock_land_share_ha",
  "fish_area",
  "fish_area_ha",
  "distance_farmland",
  "distance_freshwater",
  "distance_school",
  "distance_hospital",
  "distance_livestock",
  "distance_crops",
  "distance_transport",
  "distance_road",
  "cars",
  "motorbikes",
  "bicycles",
  "gas_cookers",
  "electric_cookers",
  "mobile_phones",
  "smartphones",
  "ox_plough",
  "tractors",
  "plows",
  "seed_drills",
  "crop_facilities",
  "asset_other_count",
  "area_at_threat",
  "area_at_threat_ha"
)

main_surveys <- main_surveys%>%
  mutate_at(all_of(missing_vars_main), function(x) ifelse(x %in% missing_codes, NA, x))

main_surveys <- main_surveys%>%
  mutate(
    chem_fert_applied_kg = ifelse(is.na(chem_fert_applied),NA, chem_fert_applied_kg),
    chem_fert_area_ha = ifelse(is.na(chem_fert_area),NA, chem_fert_area_ha),
    chem_fert_applied_per_area = ifelse(is.na(chem_fert_applied) | is.na(chem_fert_area),NA, chem_fert_applied_per_area),
    chem_fert_kg_ha = ifelse(is.na(chem_fert_applied)  | is.na(chem_fert_area),NA, chem_fert_kg_ha),
    
    own_organic_fert_applied_kg = ifelse(is.na(own_organic_fert_applied),NA, own_organic_fert_applied_kg),
    own_organic_fert_area_ha = ifelse(is.na(own_organic_fert_area),NA, own_organic_fert_area_ha),
    own_organic_fert_applied_per_area = ifelse(is.na(own_organic_fert_applied) | is.na(own_organic_fert_area),NA, own_organic_fert_applied_per_area),
    own_organic_fert_kg_ha = ifelse(is.na(own_organic_fert_applied)  | is.na(own_organic_fert_area),NA, own_organic_fert_kg_ha),
    
    bought_organic_fert_applied_kg = ifelse(is.na(bought_organic_fert_applied),NA, bought_organic_fert_applied_kg),
    bought_organic_fert_area_ha = ifelse(is.na(bought_organic_fert_area),NA, bought_organic_fert_area_ha),
    bought_organic_fert_applied_per_area = ifelse(is.na(bought_organic_fert_applied) | is.na(bought_organic_fert_area),NA, bought_organic_fert_applied_per_area),
    bought_organic_fert_kg_ha = ifelse(is.na(bought_organic_fert_applied)  | is.na(bought_organic_fert_area),NA, bought_organic_fert_kg_ha),
    
    chemical_applied_kg = ifelse(is.na(chemical_applied),NA, chemical_applied_kg),
    chemical_area_ha = ifelse(is.na(chemical_area),NA, chemical_area_ha),
    chemical_applied_per_area = ifelse(is.na(chemical_applied) | is.na(chemical_area),NA, chemical_applied_per_area),
    chemical_kg_ha = ifelse(is.na(chemical_applied)  | is.na(chemical_area),NA, chemical_kg_ha),
    
    non_chemical_applied_kg = ifelse(is.na(non_chemical_applied),NA, non_chemical_applied_kg),
    non_chemical_area_ha = ifelse(is.na(non_chemical_area),NA, non_chemical_area_ha),
    non_chemical_applied_per_area = ifelse(is.na(non_chemical_applied) | is.na(non_chemical_area),NA, non_chemical_applied_per_area),
    non_chemical_kg_ha = ifelse(is.na(non_chemical_applied)  | is.na(non_chemical_area),NA, non_chemical_kg_ha),
    
    total_crop_area_ha = ifelse(is.na(total_crop_area), NA, total_crop_area),
    livestock_land_own_ha = ifelse(is.na(livestock_land_own), NA, livestock_land_own_ha),
    livestock_land_share_ha = ifelse(is.na(livestock_land_share), NA, livestock_land_share_ha),
    fish_area_ha = ifelse(is.na(fish_area), NA, fish_area_ha),
    area_at_threat_ha = ifelse(is.na(area_at_threat), NA, area_at_threat_ha)
    )

#permanent_workers <- permanent_workers%>%
#  mutate(perm_labourer_numbers = ifelse(perm_labourer_numbers %in% missing_codes, NA, perm_labourer_numbers))

seasonal_workers <- seasonal_workers%>%
  mutate(seasonal_labour_n_working = ifelse(seasonal_labour_n_working %in% missing_codes, NA, seasonal_labour_n_working))

ecological_practices <- ecological_practices%>%
  mutate(practice_area_ha = ifelse(is.na(practice_area),NA, practice_area_ha))

crops <- crops%>%
  mutate(yield_kg = ifelse(is.na(total_yield),NA, yield_kg),
         primary_crop_area_ha = ifelse(is.na(primary_crop_area),NA, primary_crop_area_ha),
         yield_weight_area = ifelse(is.na(total_yield) | is.na(primary_crop_area_ha),NA, yield_weight_area),
         yield_kg_ha = ifelse(is.na(total_yield) | is.na(primary_crop_area_ha),NA, yield_kg))

################################################################################
# GET REFERNCE DATASETS
################################################################################
#ref_cli_mitigation <- read.csv("reference data/climate_mitigation.csv")
ref_cli_mitigation <- dbGetQuery(con,"SELECT * FROM climate_mitigation_scores")
#ref_income <- read.csv("reference data/income.csv")
ref_income <- dbGetQuery(con,"SELECT * FROM gni_entries")
ref_crops <- dbGetQuery(con,"SELECT * FROM crop_list_entries")

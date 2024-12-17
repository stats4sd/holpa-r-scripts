library(RMariaDB)
library(ssh)
library(sys)
library(tidyverse)
library(jsonlite)
library(dotenv)
library(data.table)

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

  datadesc_query <- paste0("describe ", table)
  getdata_query <- paste0("from ", table)
  
  data_desc <- setDT(dbGetQuery(con,datadesc_query)) #get rest of data
  other_cols <- data_desc[Type!="json",Field] #remove JSONS
  data <- dbGetQuery(con,paste("select",paste(other_cols,collapse = ", "),getdata_query))
  
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

main_survey <- get_dataset("main_surveys")
products <- get_dataset("products")
permanent_workers <- get_dataset("permanent_workers")
seasonal_workers <- get_dataset("seasonal_workers")
ecological_practices <- dbGetQuery(con,"SELECT * FROM ecological_practices")
crops <- get_dataset("crops")
livestock <- get_dataset("livestock")
livestock_uses <- get_dataset("livestock_uses")
fish <- get_dataset("fish")
fish_uses <- get_dataset("fish_uses")
fieldwork_sites <- get_dataset("fieldwork_sites")

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

main <- main%>%
  mutate_at(all_of(missing_vars_main), function(x) ifelse(x %in% missing_codes, NA, x))

main%>%
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

permanent_workers <- permanent_workers%>%
  mutate(perm_labourer_numbers = ifelse(perm_labourer_numbers %in% missing_code, NA, perm_labourer_numbers))

seasonal_workers <- seasonal_workers%>%
  mutate(seasonal_labourer_n_working = ifelse(seasonal_labourer_n_working %in% missing_code, NA, seasonal_labourer_n_working))

ecological_practices <- ecological_practices%>%
  mutate(practice_area_ha = ifelse(is.na(practice_area),NA, chem_fert_applied_kg))

crops <- crops%>%
  mutate(yield_kg = ifelse(is.na(total_yield),NA, yield_kg),
         primary_crop_area_ha = ifelse(is.na(primary_crop_area),NA, primary_crop_area_ha),
         yield_weight_area = ifelse(is.na(total_yield) | is.na(primary_crop_area_ha),NA, yield_weight_area),
         yield_kg_ha = ifelse(is.na(total_yield) | is.na(primary_crop_area_ha),NA, yield_kg))

################################################################################
# GET REFERNCE DATASETS
################################################################################

# ref_cli_mitigation <- dbGetQuery(con,"SELECT * FROM ref_cli_mitigation")
# ref_income <- dbGetQuery(con,"SELECT * FROM ref_income")
# ref_yield <- dbGetQuery(con,"SELECT * FROM ref_yield")
# ref_fertiliser <- dbGetQuery(con,"SELECT * FROM ref_fertiliser")
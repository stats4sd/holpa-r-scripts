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

can.be.numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  return(numNAs_new == numNAs)
}

na_99 <- function(data){
  
  data <- data%>%
    mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x))
  
  return(data)
  
}

#Convert back to numbers as variables are otherwise presented as characters
number_fix <- function(data){
  
  #convert "NA" or "NaN" to NA proper
  data <- data%>%
    mutate_all(function(x) ifelse(x == "NA", NA, x))%>%
    mutate_all(function(x) ifelse(x == "NaN", NA, x))
  
  data <- as.data.frame(lapply(data, function(col) {
    if (can.be.numeric(col)) {
      as.numeric(col)
    } else {
      col
    }
  }))
  
  data <- na_99(data)
  
  return(data)
  
}

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

entity_values <- dbGetQuery(con, "SELECT * FROM entity_values")
entities <- dbGetQuery(con, "SELECT * FROM entities")
teams <- dbGetQuery(con, "SELECT * FROM teams")

entity_values <- entity_values%>%left_join(entities%>%select(id,owner_id), by = c("entity_id" = "id"))

# FARM SURVEY DATA

farm_survey_ids <- entities$id[entities$dataset_id==1]

fieldwork_ids <- entities$id[entities$dataset_id == 16]

farm_ids <- entity_values%>%
  filter((entity_id %!in% farm_survey_ids & entity_id %!in% fieldwork_ids) | dataset_variable_name=="farm_id")%>%
  group_by(entity_id)%>%
  slice(1)%>%
  left_join(entities%>%select(id, submission_id), by = c("entity_id" = "id"))%>%
  group_by(submission_id)%>%
  mutate(farm_id = value[dataset_variable_name=="farm_id"])%>%
  select(entity_id, submission_id, farm_id, owner_id)%>%
  ungroup()

main_surveys <- entity_values%>%
  filter(entity_id %in% farm_survey_ids)%>%
  group_by(entity_id)%>%
  mutate(farm_id = value[dataset_variable_name=="farm_id"])%>%
  filter(dataset_variable_name!="farm_id")%>%
  relocate(farm_id, .before = id)%>%
  select(-id)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(farm_id, owner_id), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()%>%
  left_join(farm_ids%>%filter(entity_id%in%farm_survey_ids))

# CROPS

crop_data_ids <- entities$id[entities$dataset_id==2]

crops <- entity_values%>%
  filter(entity_id %in% crop_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(primary_crop_number = value[dataset_variable_name=="primary_crop_number"])%>%
  filter(dataset_variable_name!="primary_crop_number")%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id, primary_crop_number), names_from = dataset_variable_name, values_from = value)

# Ecological practices

eco_data_ids <- entities$id[entities$dataset_id==3]

ecological_practices <- entity_values%>%
  filter(entity_id %in% eco_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(practice_number = value[dataset_variable_name=="practice_number"])%>%
  filter(dataset_variable_name!="practice_number")%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id, practice_number), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()

# FISH

fish_data_ids <- entities$id[entities$dataset_id==6]

fish <- entity_values%>%
  filter(entity_id %in% fish_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(fish_id = value[dataset_variable_name=="fish_id"])%>%
  filter(dataset_variable_name!="fish_id")%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id, fish_id), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()


# FISH

fish_use_data_ids <- entities$id[entities$dataset_id==7]

fish_uses <- entity_values%>%
  filter(entity_id %in% fish_use_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(fish_use_name = value[dataset_variable_name=="fish_use_name"])%>%
  filter(dataset_variable_name!="fish_use_name")%>%
  left_join(entities%>%select(id, parent_id), by = c("entity_id" = "id"))%>%
  left_join(entity_values%>%filter(dataset_variable_name=="fish_id")%>%select(entity_id,"fish_id" = value),
            by = c("parent_id" = "entity_id"))%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id,fish_id,fish_use_name), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()


# livestock

livestock_data_ids <- entities$id[entities$dataset_id==8]

livestock <- entity_values%>%
  filter(entity_id %in% livestock_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(livestock_id = value[dataset_variable_name=="livestock_id"])%>%
  filter(dataset_variable_name!="livestock_id")%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id, livestock_id), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()


# livestock

livestock_use_data_ids <- entities$id[entities$dataset_id==9]

livestock_uses <- entity_values%>%
  filter(entity_id %in% livestock_use_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(livestock_use_name = value[dataset_variable_name=="livestock_use_name"])%>%
  filter(dataset_variable_name!="livestock_use_name")%>%
  left_join(entities%>%select(id, parent_id), by = c("entity_id" = "id"))%>%
  left_join(entity_values%>%filter(dataset_variable_name=="livestock_id")%>%select(entity_id,"livestock_id" = value),
            by = c("parent_id" = "entity_id"))%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id,livestock_id,livestock_use_name), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()

# Permanent workers

perm_worker_data_ids <- entities$id[entities$dataset_id==11]

permanent_workers <- entity_values%>%
  filter(entity_id %in% perm_worker_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, submission_id, owner_id, entity_id), names_from = dataset_variable_name, values_from = value)%>%
  ungroup()

# Products

products_data_ids <- entities$id[entities$dataset_id==12]

products <- entity_values%>%
  filter(entity_id %in% products_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(product_name = value[dataset_variable_name=="product_name"])%>%
  filter(dataset_variable_name!="product_name")%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, product_name), names_from = dataset_variable_name, values_from = value)

# Seasonal workers

seasonal_worker_data_ids <- entities$id[entities$dataset_id==13]

seasonal_workers <- entity_values%>%
  filter(entity_id %in% seasonal_worker_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, entity_id), names_from = dataset_variable_name, values_from = value)

# SITES

site_data_ids <- entities$id[entities$dataset_id==15]

sites <- entity_values%>%
  filter(entity_id %in% site_data_ids)%>%
  left_join(farm_ids)%>%
  group_by(entity_id)%>%
  mutate(site_no = value[dataset_variable_name=="site_no"])%>%
  filter(dataset_variable_name!="site_no")%>%
  select(-id)%>%
  pivot_wider(id_cols = c(farm_id, owner_id, submission_id, site_no), names_from = dataset_variable_name, values_from = value)

#################################################################################
# FIX TO NUMERIC
#################################################################################

main_surveys <- number_fix(main_surveys)
crops <- number_fix(crops)
ecological_practices <- number_fix(ecological_practices)
fish <- number_fix(fish)
fish_uses <- number_fix(fish_uses)
livestock <- number_fix(livestock)
livestock_uses <- number_fix(livestock_uses)
permanent_workers <- number_fix(permanent_workers)
seasonal_workers <- number_fix(seasonal_workers)
products <- number_fix(products)
sites <- number_fix(sites)


missing_codes <- c(99,999,9999,99999, 888, 8888, 8888, 555, 5555, 55555, 777, 7777, 77777)

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

missing_vars_main <- missing_vars_main[missing_vars_main%in%colnames(main_surveys)]

main_surveys <- main_surveys%>%
  mutate_at(vars(all_of(missing_vars_main)), function(x) ifelse(x %in% missing_codes, NA, x))

# possibly repeat for all tables

# TRY TO AUTOMATE SECTION BELOW
if("chem_fert_applied" %in% colnames(main_surveys)){
main_surveys <- main_surveys%>%
  mutate(
    chem_fert_applied_kg = ifelse(is.na(chem_fert_applied),NA, chem_fert_applied_kg),
    chem_fert_area_ha = ifelse(is.na(chem_fert_area),NA, chem_fert_area_ha),
    chem_fert_applied_per_area = ifelse(is.na(chem_fert_applied) | is.na(chem_fert_area),NA, chem_fert_applied_per_area),
    chem_fert_kg_ha = ifelse(is.na(chem_fert_applied)  | is.na(chem_fert_area),NA, chem_fert_kg_ha)
  )
    }

if("own_organic_fert_applied" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(
    own_organic_fert_applied_kg = ifelse(is.na(own_organic_fert_applied),NA, own_organic_fert_applied_kg),
    own_organic_fert_area_ha = ifelse(is.na(own_organic_fert_area),NA, own_organic_fert_area_ha),
    own_organic_fert_applied_per_area = ifelse(is.na(own_organic_fert_applied) | is.na(own_organic_fert_area),NA, own_organic_fert_applied_per_area),
    own_organic_fert_kg_ha = ifelse(is.na(own_organic_fert_applied)  | is.na(own_organic_fert_area),NA, own_organic_fert_kg_ha)
    )
}

if("bought_organic_fert_applied" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(
    bought_organic_fert_applied_kg = ifelse(is.na(bought_organic_fert_applied),NA, bought_organic_fert_applied_kg),
    bought_organic_fert_area_ha = ifelse(is.na(bought_organic_fert_area),NA, bought_organic_fert_area_ha),
    bought_organic_fert_applied_per_area = ifelse(is.na(bought_organic_fert_applied) | is.na(bought_organic_fert_area),NA, bought_organic_fert_applied_per_area),
    bought_organic_fert_kg_ha = ifelse(is.na(bought_organic_fert_applied)  | is.na(bought_organic_fert_area),NA, bought_organic_fert_kg_ha)
    )
}

if("chemical_applied" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(
    chemical_applied_kg = ifelse(is.na(chemical_applied),NA, chemical_applied_kg),
    chemical_area_ha = ifelse(is.na(chemical_area),NA, chemical_area_ha),
    chemical_applied_per_area = ifelse(is.na(chemical_applied) | is.na(chemical_area),NA, chemical_applied_per_area),
    chemical_kg_ha = ifelse(is.na(chemical_applied)  | is.na(chemical_area),NA, chemical_kg_ha)
    )
}

if("non_chemical_applied" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(   
    non_chemical_applied_kg = ifelse(is.na(non_chemical_applied),NA, non_chemical_applied_kg),
    non_chemical_area_ha = ifelse(is.na(non_chemical_area),NA, non_chemical_area_ha),
    non_chemical_applied_per_area = ifelse(is.na(non_chemical_applied) | is.na(non_chemical_area),NA, non_chemical_applied_per_area),
    non_chemical_kg_ha = ifelse(is.na(non_chemical_applied)  | is.na(non_chemical_area),NA, non_chemical_kg_ha)
    )
}

if("total_crop_area" %in% colnames(main_surveys)){
main_surveys <- main_surveys%>%
  mutate(   
    total_crop_area_ha = ifelse(is.na(total_crop_area), NA, total_crop_area))
}

if("livestock_land_own" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(   
      livestock_land_own_ha = ifelse(is.na(livestock_land_own), NA, livestock_land_own_ha),
      livestock_land_share_ha = ifelse(is.na(livestock_land_share), NA, livestock_land_share_ha))
}

if("fish_area" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(   
      fish_area_ha = ifelse(is.na(fish_area), NA, fish_area_ha))
}

if("area_at_threat" %in% colnames(main_surveys)){
  main_surveys <- main_surveys%>%
    mutate(   
      area_at_threat_ha = ifelse(is.na(area_at_threat), NA, area_at_threat_ha))
}

permanent_workers <- permanent_workers%>%
  mutate(perm_labourer_numbers = ifelse(perm_labourer_numbers %in% missing_codes, NA, perm_labourer_numbers))
 
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

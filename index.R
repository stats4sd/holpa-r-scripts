library(dotenv)

############################################
# Get Data
############################################

env_path <- paste(getwd(), ".env", sep = '/')
dotenv::load_dot_env(env_path)

if (Sys.getenv('data_source') == 'db') {
  source("data_processing/get_data_from_db.R")
} else {
  source("data_processing/get_data_from_excel.R")
}

############################################
# Check that the required data frames exist
############################################

if (
 !exists("main_surveys") ||
 !exists("crops") ||
 !exists("ecological_practices") ||
 !exists("fish") ||
 !exists("fish_uses") ||
 !exists("livestock") ||
 !exists("livestock_uses") ||
 !exists("permanent_workers") ||
 !exists("seasonal_workers") ||
 !exists("products") ||
 !exists("sites")
) {
  stop(
    "You do not have the required data frames. Please check that the get_data script has been run and did not give errors."
  )
}

############################################
# RUN CALCULATION SCRIPTS
############################################

source('data_processing/holpa_agroecology_scores.R')
source('data_processing/key_performance_indicators.R')

agroecology_scores <- calculate_agroecology_scores()
performance_indicators <- calculate_key_performance_indicators()



############################################
# EXPORT CALCULATED INDICATORS
# Exports to the database or Excel file, depending on data source
############################################

if (Sys.getenv('data_source') == 'db') {
  source("data_processing/write_data_to_db.R")

  write_ae_scores_to_db()
  write_kpis_to_db()

} else {

  source("data_processing/write_data_to_excel.R")

  write_data_to_excel()

}

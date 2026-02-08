



################################################################################
# GET REFERNCE DATASETS
################################################################################
ref_cli_mitigation <- read.csv('reference_data/climate_mitigation')
ref_income <- read.csv('reference_data/income.csv')
ref_crops <- read.csv('reference_data/yields.csv')

ref_crops <- number_fix(ref_crops)

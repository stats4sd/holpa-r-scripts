# HOLPA Analysis Scripts

This repository contains the R scripts required to calculate the Agroecological and Key Performance Indicators from data collected using the HOLPA surveys.

These scripts assume your data has been pre-processed slightly and put into a format more suited for storage and analysis than what you get directly from the ODK forms. If you are using the HOLPA data platform at https://holpa.org and running your surveys through the built-in ODK Central service, this pre-processing occurs automatically for you, and you can export your data to Excel such that it's immediately ready for further analysis. If you are doing data collection through other tools (e.g., KoboToolbox, Ona.io, SurveyCTO) you will need to do some extra steps.

> TODO: add instructions on how to get raw ODK data into correct format. (Describe format and steps)

## How to use these scripts

### 1. Setup

To use these scripts, follow this setup process:

1. clone or download the repository to your local computer. You can download the whole repo as a .zip file by clicking "Code" and "Download ZIP", or follow the instructions to clone the repository via HTTPS or SSH.
   - e.g.: `git clone https://github.com/stats4sd/hopla-r-scripts`

2. Now you have the project folder locally, you should create a ".env" file. Copy the `.env.exmple` file and name it `.env`.

3. If you have your data in an Excel file, you should copy that data file into the "data" folder, and update the `DATA_FILE_PATH` variable to point to your file. Make sure to keep the `data/` part of the file path.

4. If you have your data in a MySQL database (e.g. you have a copy of the HOLPA Data platform database stored locally), you should update the `DATA_SOURCE` variable to `db` and check the database connection variables are correct.

5. To run the whole process to calculate the agroecology scores and key performance indicators, run the `index.R` script.

To do the steps manually, follow the next steps here.

## 2. Get your data

There are two sets of setup scripts that import your data into R.

1. If you have your data in Excel, use the `get_data_from_excel.R` script to import your data.
   1. Make sure you have copied your Excel file into the `data/` folder and have updated the `DATA_FILE_PATH` env. variable.
2. If you have your data in a MySQL database, e.g. a clone of the data platform's database, use the `get_data_from_db.R` script.

> [!NOTE]
> If you are running these scripts manually, you probably have your data in Excel. The database scripts are included because they are used on the HOLPA data platform, and may be of interest if you are developing your own data processing pipeline using MySQL to store your HOLPA data.

## 2. Calculate the Agroecology Scores + KPIs

Once you have imported your data to R with the correct script, you should have the following data frames available in your R session:

- `main_surveys` - the main survey data from the household survey.
- `crops` - data about the primary crops grown. Linked to the `main_surveys` via the farm_id.
- `ecological_practices` - data about the specific ecological practices used on the farm. Linked to the `main_surveys` via the farm_id.
- `fish` - data about the primary fish farmed. Linked to the `main_surveys` via the farm_id.
- `fish_uses` - data about the uses of each type of primary fish. Linked to `fish` via fish_id.
- `livestock` - data about the primary livestock reared on the farm. Linked to the `main_surveys` via the farm_id.
- `livestock_uses` - data about the uses of each primary livestock type. Linked to `livestock` via livestock_id.
- `permanent_workers` - data about the permanent workers and labourers on the farm. Linked to the `main_surveys` via the farm_id.
- `seasonal_workers` - data about the seasonal workers and labourers on the farm. Linked to the `main_surveys` via the farm_id.
- `products` - data about the different products produced by the farm. (Trees, honey, etc). Linked to the `main_surveys` via the farm_id.
- `sites` - the main farm site data from the fieldwork survey. There should be 3 entries per farm; linked to the main_surveys data via farm_id.

You can then run the functions in the `holpa_agroecology_scores.R` and `key_performance_indicators.R` script files. E.g:

```R

source('data_processing/holpa_agroecology_scores.R')
source('data_processing/key_performance_indicators.R')

agroecology_scores <- calculate_agroecology_scores()
performance_indicators <- calculate_key_performance_indicators()

```

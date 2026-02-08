library(tidyverse)
library(RMariaDB)

################################################################################
# WRITE AE SCORES TO DATBASE
###############################################################################

write_ae_scores_to_db <- function() {
  source('data_processing/helper_functions.R')
  con <- get_db()

  agroecology_scores <- agroecology_scores %>%
    mutate(id = row_number()) %>%
    mutate_at(vars(recycling_1_score:participation_label), as.character) %>%
    pivot_longer(cols = -c(id, farm_id, owner_id, submission_id)) %>%
    group_by(id, farm_id, owner_id, submission_id) %>%
    mutate(value = replace_na(value, "NA")) %>%
    summarise(
      properties = jsonlite::toJSON(data.table::transpose(
        cur_data(),
        make.names = TRUE
      ))
    ) %>%
    mutate(properties = str_remove_all(properties, "\\["))

  agroecology_scores <- agroecology_scores %>%
    mutate(properties = str_remove_all(properties, "\\]"))

  dbWriteTable(
    con,
    "agroecology_scores",
    agroecology_scores,
    overwrite = TRUE
  )

  dbDisconnect(con)
}

############################################
# WRITE KPIs TO DATABASE
############################################

write_kpis_to_db <- function() {
  source('data_processing/helper_functions.R')
  con <- get_db()

  performance_indicators <- performance_indicators %>%
    mutate(id = row_number()) %>%
    mutate_at(
      vars(kpi1a_crop_health:kpi18_human_wellbeing_scaled),
      as.character
    ) %>%
    pivot_longer(cols = -c(id, farm_id, owner_id, submission_id)) %>%
    group_by(id, farm_id, owner_id, submission_id) %>%
    mutate(value = replace_na(value, "NA")) %>%
    summarise(
      properties = jsonlite::toJSON(data.table::transpose(
        cur_data(),
        make.names = TRUE
      ))
    ) %>%
    mutate(properties = str_remove_all(properties, "\\["))

  performance_indicators <- performance_indicators %>%
    mutate(properties = str_remove_all(properties, "\\]"))

  dbWriteTable(
    con,
    "performance_indicators",
    performance_indicators,
    overwrite = TRUE
  )

  dbDisconnect(con)
}

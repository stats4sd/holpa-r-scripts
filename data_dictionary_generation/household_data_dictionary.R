library(tidyverse)
library(openxlsx)

source("data_processing/get_db_connection.R")

survey_sheet <- read.xlsx("metadata/HOLPA Household.xlsx", sheet = 1,cols = 9:24)

dd <- survey_sheet%>%
  mutate(
    worksheet = ifelse(is.na(repeat_flag), "Main_Survey",
                       paste0("repeat_", repeat_group_name))
  )%>%
  select(
    worksheet,
    "variable" = name,
    "label_question" = `label::English.(en)`,
    type,
    calculation
  )%>%
  filter(!str_detect(type, "begin|end|note"))%>%
  mutate(type = trimws(type))%>%
  separate_wider_delim(type, delim = " ", names = c("type", "codelist"), 
                       too_few = "align_start")

code_lists <- read.xlsx("metadata/HOLPA Household.xlsx", sheet = 2, cols = 2:4)%>%
  select(
    codelist = list_name,
    value = name,
    label = `label::English.(en)`
  )%>%
  mutate(value = str_replace_all(value, "\\.0", ""))

multiple_selects <- inner_join(
  dd%>%filter(type == "select_multiple"),
  code_lists,
  relationship = "many-to-many"
  )%>%
  mutate(variable = paste(variable, value, sep = "."))%>%
  rename("multiple_choice_option_label" = label)%>%
  select(-value)

dd <- bind_rows(dd, multiple_selects)

dbWriteTable(con,"raw_global_code_lists",code_lists,overwrite=TRUE)
dbWriteTable(con,"raw_global_data_dictionary",dd,overwrite=TRUE)

write.xlsx(dd, "dictionaries/Raw global data dictionary.xlsx")
write.xlsx(code_lists, "dictionaries/Raw global codelists.xlsx")

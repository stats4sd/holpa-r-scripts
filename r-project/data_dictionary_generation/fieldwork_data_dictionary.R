library(tidyverse)
library(openxlsx)

source("data_processing/get_db_connection.R")

survey_sheet <- read.xlsx("metadata/HOLPA Fieldwork.xlsx", sheet = 1,cols = 2:20)

dd <- survey_sheet%>%
  mutate(
    worksheet = ifelse(is.na(repeat_group_name), "Fieldwork_survey",
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

code_lists <- read.xlsx("metadata/HOLPA Fieldwork.xlsx", sheet = 2)%>%
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

dbWriteTable(con,"raw_fieldwork_code_lists",code_lists,overwrite=TRUE)
dbWriteTable(con,"raw_fieldwork_data_dictionary",dd,overwrite=TRUE)

write.xlsx(dd, "dictionaries/Raw fieldwork data dictionary.xlsx")
write.xlsx(code_lists, "dictionaries/Raw fieldwork codelists.xlsx")

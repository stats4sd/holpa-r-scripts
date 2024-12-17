library(tidyverse)
library(openxlsx)

#source("data_processing/get_db_connection.R")

survey_sheet <- read.xlsx("metadata/HOLPA Household.xlsx", sheet = 1,cols = 9:24)
data_structures <- read.xlsx("metadata/Data Structures.xlsx")

field_work_sheet <- read.xlsx("metadata/HOLPA Fieldwork.xlsx", sheet = 1,cols = 2:20)

dd <- survey_sheet%>%
  select(-repeat_flag, -repeat_group_name)%>%
  filter(!str_detect(type, "begin|end|note"))%>%
  full_join(data_structures%>%select(Dataset, variable)%>%filter(Dataset!="Fieldwork Sites"), by = c("name" = "variable"))

unused_vars <- dd%>%filter(is.na(Dataset))

dd <- dd%>%
  filter(!is.na(Dataset))

dd <- dd%>%
  select(
    "worksheet" = Dataset,
    "variable" = name,
    "label_question" = `label::English.(en)`,
    type,
    calculation
  )%>%
  mutate(type = trimws(type))%>%
  separate_wider_delim(type, delim = " ", names = c("type", "codelist"), 
                       too_few = "align_start")

dd2 <- field_work_sheet%>%
  select(-repeat_group_name)%>%
  filter(!str_detect(type, "begin|end|note"))%>%
  full_join(data_structures%>%select(Dataset, variable)%>%filter(Dataset=="Fieldwork Sites"), by = c("name" = "variable"))

unused_vars2 <- dd2%>%filter(is.na(Dataset))

dd2 <- dd2%>%
  filter(!is.na(Dataset))

dd2 <- dd2%>%
  select(
    "worksheet" = Dataset,
    "variable" = name,
    "label_question" = `label::English.(en)`,
    type,
    calculation
  )%>%
  mutate(type = trimws(type))%>%
  separate_wider_delim(type, delim = " ", names = c("type", "codelist"), 
                       too_few = "align_start")

dd <- bind_rows(dd,dd2)

code_lists <- read.xlsx("metadata/HOLPA Household.xlsx", sheet = 2, cols = 2:4)%>%
  select(
    codelist = list_name,
    value = name,
    label = `label::English.(en)`
  )%>%
  mutate(value = str_replace_all(value, "\\.0", ""))%>%
  group_by(codelist, value)%>%
  unique()

multiple_selects <- inner_join(
  dd%>%filter(type == "select_multiple"),
  code_lists,
  relationship = "many-to-many"
  )%>%
  mutate(variable = paste(variable, value, sep = "_"))%>%
  rename("multiple_choice_option_label" = label)%>%
  select(-value)

dd <- bind_rows(dd, multiple_selects)

write.xlsx(dd, "dictionaries/Raw global data dictionary.xlsx")
write.xlsx(code_lists, "dictionaries/Raw global codelists.xlsx")

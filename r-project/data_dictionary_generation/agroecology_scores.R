library(tidyverse)
library(openxlsx)

#source("data_processing/get_db_connection.R")

ag_scores <- read.xlsx("metadata/agroecology_indicators.xlsx")

data_dictionary <- ag_scores%>%
  select(principle,
         indicator_no, 
         "main_survey_variable" = new_name, 
         "old_qno" = name_question_recla,
         "label_question" = question,
)%>%
  group_by(principle, indicator_no)%>%
  slice(1:2)%>%
  mutate(principle_no = parse_number(principle))

data_dictionary$score_label <- rep(c("score", "label"),60)
data_dictionary$type <- rep(c("numeric (1 - 5)", "character"),60)

data_dictionary$principle_name <-
  case_when(
    data_dictionary$principle_no == 1 ~ "recycling",
    data_dictionary$principle_no == 2 ~ "input_reduction",
    data_dictionary$principle_no == 3 ~ "soil_health",
    data_dictionary$principle_no == 4 ~ "animal_health",
    data_dictionary$principle_no == 5 ~ "biodiversity",
    data_dictionary$principle_no == 6 ~ "synergy",
    data_dictionary$principle_no == 7 ~ "enconomic_diversification",
    data_dictionary$principle_no == 8 ~ "co_creation_knowledge",
    data_dictionary$principle_no == 9 ~ "social_values_diet",
    data_dictionary$principle_no == 10 ~ "fariness",
    data_dictionary$principle_no == 11 ~ "connectivity",
    data_dictionary$principle_no == 12 ~ "governance",
    data_dictionary$principle_no == 13 ~ "participation"
  )

data_dictionary$variable <- paste(data_dictionary$principle_name, data_dictionary$indicator_no, data_dictionary$score_label, sep = "_")

data_dictionary <- data_dictionary%>%
  select(-principle_no,
         -principle_name, 
         -score_label)%>%
  rename("theme" = principle)%>%
  relocate(variable, .after = indicator_no)

dbWriteTable(con,"agroecology_scores_data_dictionary",data_dictionary,overwrite=TRUE)

write.xlsx(data_dictionary, "dictionaries/Agroecology Scores data dictionary.xlsx")

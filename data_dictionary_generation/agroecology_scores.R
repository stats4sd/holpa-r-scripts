library(tidyverse)
library(openxlsx)

ag_scores <- read.xlsx("metadata/agroecology_indicators.xlsx")

data_dictionary <- ag_scores%>%
  select(principle,
         indicator_no, 
         "main_survey_variable" = new_name, 
         "old_qno" = name_question,
         "label_question" = question,
)%>%
  group_by(principle, indicator_no)%>%
  slice(1:2)%>%
  mutate(principle_no = parse_number(principle))

data_dictionary$score_label <- rep(c("score", "label"),60)

data_dictionary$principle_name <-
  case_when(
    data_dictionary$principle_no == 1 ~ "recycling",
    data_dictionary$principle_no == 2 ~ "input_reduction",
    data_dictionary$principle_no == 3 ~ "soil_health",
    data_dictionary$principle_no == 4 ~ "animal_health",
    data_dictionary$principle_no == 5 ~ "biodiversity",
    data_dictionary$principle_no == 6 ~ "recycling",
    data_dictionary$principle_no == 7 ~ "recycling",
    data_dictionary$principle_no == 8 ~ "recycling",
    data_dictionary$principle_no == 9 ~ "recycling",
    data_dictionary$principle_no == 10 ~ "recycling",
    data_dictionary$principle_no == 11 ~ "recycling",
    data_dictionary$principle_no == 12 ~ "recycling",
    data_dictionary$principle_no == 13 ~ "recycling"
  )

library(tidyverse)
library(lubridate)
library(glue)

# read data 
df_tool_data <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Questionnaire_for_Sampled_Households.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.settlement = settlement,
         start = as_datetime(start),
         end = as_datetime(end))

# log
df_log <- readxl::read_excel("inputs/combined_checks_IPE_questionnaire_for_sampled_households.xlsx")

# tool
df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "choices")

# average survey time

average_survey_time <- df_tool_data %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval)
         )%>% 
  select(int.survey_time_interval) %>% 
  summarise(average_time = mean(int.survey_time_interval, na.rm = TRUE))
  

# summary from the logs reviewed ------------------------------------------

# number of surveys with comment "option already exists"

df_log %>% 
  filter(str_detect(string = comment, pattern = fixed('already exist', ignore_case = TRUE))) %>% 
  group_by(uuid) %>% 
  select(uuid) %>% 
  unique()

# most appearing questions
# df_survey_modified <- df_survey %>% 
#   mutate(qn_type = str_extract(string = type, pattern = "^\\w+\\b")) %>%  
#   select(qn_type , name)

df_log %>% 
  filter(!is.na(name),  !type %in% c("add_option")) %>% 
  # left_join(df_survey_modified, by = "name") %>% 
  group_by(name, label) %>% 
  summarise(number_of_occurances = n()) %>% 
  filter(number_of_occurances > 5) %>% 
  arrange(desc(number_of_occurances))
  
# reapeated similar text
df_log %>% 
  filter(!is.na(name), !name %in% c("gps_coordinates"), !type %in% c("add_option")) %>% 
  mutate(other_text = str_to_lower(other_text),
         other_text = str_squish(other_text)) %>% 
  group_by(name, label, other_text) %>% 
  summarise(number_of_occurances = n()) %>% 
  filter(number_of_occurances > 1) %>% 
  arrange(desc(number_of_occurances))

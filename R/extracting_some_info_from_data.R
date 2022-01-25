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
df_log <- readr::read_csv("inputs/combined_checks_IPE_questionnaire_for_sampled_households.xlsx")

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
  



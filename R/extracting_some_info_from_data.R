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

df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "choices")
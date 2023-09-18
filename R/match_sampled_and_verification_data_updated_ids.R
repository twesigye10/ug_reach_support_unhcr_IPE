library(tidyverse)
library(lubridate)
library(glue)


# verification ------------------------------------------------------------
# updated dataset with new group ids
data_path_verification <- "inputs/IPE_Updated/Reach data IPE 2908.csv"

df_ipe_data_complete_verification <- readr::read_csv(file =  data_path_verification, na = c("NA", "NULL"))
colnames(df_ipe_data_complete_verification)
  
# sampled data ------------------------------------------------------------

sampled_data_loc <- "inputs/IPE_Updated/Reach data Household Visit 2908.csv"

df_hh_sampled_data <- readr::read_csv(sampled_data_loc)
colnames(df_hh_sampled_data)


# check that the key works for both datasets ------------------------------

df_ipe_data_complete_verification |> 
  filter(AnonymizedGrp %in% df_hh_sampled_data$anonymizedgroup)

df_sample_in_verific <- df_hh_sampled_data |> 
  filter(anonymizedgroup %in% df_ipe_data_complete_verification$AnonymizedGrp)

nrow(df_hh_sampled_data)
# 22628

nrow(df_sample_in_verific)
# 21281

# difference
# 1347

colnames(df_ipe_data_complete_verification)


df_joined


# missing columns ---------------------------------------------------------

# get NA columns
df_na_cols <- df_ipe_data_complete_verification %>%  
  select(where(function(x) all(is.na(x)))) %>% 
  colnames()
# "37" "38" "41" "60"

# verification data columns
df_verific_cols <- df_ipe_data_complete_verification %>% 
  select(where(function(x) !all(is.na(x)))) %>%
  colnames()
  
# verification column mappings

# question/choices codes and labels
# df_questions <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") |> 
  # filter(!is.na(Question)) |> 
  # mutate(question_code = as.character(QuestionID),
  #        question_label = as.character(Question),
  #        question_name = as.character(name)) |> 
  # select(question_code, question_label, question_name)

df_questions <- readxl::read_excel("inputs/IPE_Updated/IPE Questionnaire Individuals mapping_updated_data.xlsx", sheet = "Sheet1") |> 
  filter(!is.na(Question)) |> 
  select(QuestionID, SubGroupID, Question)

# question codes not in verif data

df_qn_code <- df_questions %>% 
  select(QuestionID) %>% 
  pull()


columns_missing <- df_qn_code[!df_qn_code %in% df_verific_cols]

df_missing_indicators <- df_questions %>% 
  filter(QuestionID %in% columns_missing, !QuestionID %in% c("96"))
  

openxlsx::write.xlsx(x = df_missing_indicators,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_missing_indicators_ipe_verification.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

openxlsx::openXL(file = paste0("outputs/", butteR::date_file_prefix(), 
                               "_missing_indicators_ipe_verification.xlsx"))

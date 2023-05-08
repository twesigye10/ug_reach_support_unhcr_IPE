library(tidyverse)
library(srvyr)
library(supporteR)

source("R/composite_indicators.R")
source("R/make_weights.R")

# clean data
data_path <- "inputs/REACH DataPWD/combined_ipe_verif_data_with_batch_name.csv"

df_main_clean_data <- readr::read_csv(file =  data_path)

# question/choices codes and labels
df_questions <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") |> 
  filter(!is.na(Question)) |> 
  mutate(question_code = as.character(QuestionID),
         question_label = as.character(Question),
         question_name = as.character(name)
  ) |> 
  select(question_code, question_label, question_name)

df_choices <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") |> 
  mutate(choice_code = as.character(AnswerID),
         choice_label = as.character(Answer)) |> 
  select(choice_code, choice_label)

qn_label_lookup <- setNames(object = df_choices$choice_label, nm = df_choices$choice_code)


# dap
dap <- read_csv("inputs/r_dap_ipe_sev_verification.csv")
df_ref_pop <- read_csv("inputs/refugee_population_ipe.csv")

# make composite indicator ------------------------------------------------

df_questions_dap <- df_questions |> 
  filter(question_name %in% dap$variable)

df_with_composites <- df_main_clean_data |> 
  rename(any_of(setNames(df_questions_dap$question_code, df_questions_dap$question_name))) |> 
  create_composites_verification_sev() |>  
  mutate(strata = paste0(settlement, "_refugee"))

# create weights ----------------------------------------------------------

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_with_composites, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_with_composites |>  
  left_join(ref_weight_table, by = "strata")


# set up design object ----------------------------------------------------

 
ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights)


# analysis ----------------------------------------------------------------

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap)
# merge analysis

combined_analysis <- df_main_analysis

# add labels
full_analysis_labels <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         select_type = "select_one") |> 
  mutate(variable_val_label = recode(variable_val, !!!qn_label_lookup))

# convert to percentage
full_analysis_long <- full_analysis_labels |> 
  mutate(`mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  select(`Question`= variable, 
         variable, 
         `choices/options` = variable_val, 
         `choices/options label` = variable_val_label, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_ipe_verification_sev.csv"), na="")

library(tidyverse)
library(srvyr)
library(supporteR)

source("R/composite_indicators.R")
source("R/make_weights.R")


# question/choices codes and labels
df_questions <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") %>% 
  filter(!is.na(Question)) %>% 
  mutate(question_code = as.character(QuestionID),
         question_label = as.character(Question),
         question_name = as.character(name)
  ) %>% 
  select(question_code, question_label, question_name)

df_choices <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") %>% 
  mutate(choice_code = as.character(AnswerID),
         choice_label = as.character(Answer)) %>% 
  select(choice_code, choice_label)

choice_label_lookup <- setNames(object = df_choices$choice_label, nm = df_choices$choice_code)

# dap
dap_verification <- read_csv("inputs/r_dap_ipe_verification.csv")

df_questions_dap <- df_questions %>% 
  filter(question_name %in% dap_verification$variable)

# clean HH data
data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"
data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")
df_hh_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA") %>% 
  mutate(strata = paste0(settlement, "_refugee"))

# verification data
data_path <- "inputs/combined_ipe_verif_data.csv"

df_combined_verification_data <- readr::read_csv(file =  data_path, na = "NULL") %>% 
  filter(AnonymizedGrp %in% df_hh_data$anonymizedgroup) %>% 
  rename(any_of(setNames(df_questions_dap$question_code, df_questions_dap$question_name)))  %>%  
  mutate(across(.cols = any_of(df_questions_dap$question_name), .fns = ~as.character(.x)))

# population figures
df_ref_pop <- read_csv("inputs/refugee_population_ipe.csv")

# make composite indicator ------------------------------------------------

df_with_composites_verification <- df_combined_verification_data %>% 
  create_composites_verification() %>%  
  mutate(settlement = progres_coalocationlevel2name,
         strata = paste0(settlement, "_refugee"))

# create weights ----------------------------------------------------------

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_hh_data, 
                                              input_refugee_pop = df_ref_pop)

df_ref_with_weights_verification <- df_with_composites_verification %>%  
  left_join(ref_weight_table, by = "strata")


# set up design object ----------------------------------------------------

 
ref_svy_verification <- as_survey(.data = df_ref_with_weights_verification, strata = strata, weights = weights)


# analysis ----------------------------------------------------------------

# main data
df_main_analysis_verification <- analysis_after_survey_creation(input_svy_obj = ref_svy_verification,
                                                   input_dap = dap_verification) %>% 
  mutate(level = "Individual")

# protection analysis // filter on age
ref_svy_prot <- as_survey(.data = df_ref_with_weights_verification %>% filter(progres_age > 4, progres_age < 18), 
                          strata = strata, weights = weights)
df_prot_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy_prot,
                                                   input_dap = dap_verification %>% filter(variable %in% c("avg_time_child_working_payment", 
                                                                                              "child_work_involve",
                                                                                              "children_5_17_years_working_to_support_hh_for_payment",
                                                                                              "children_engaged_child_labour"))) %>% 
  mutate(level = "Individual")

# merge analysis

combined_analysis_verification <- bind_rows(df_main_analysis_verification, df_prot_analysis)

# add labels
full_analysis_labels_verification <- combined_analysis_verification %>% 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         select_type = "select_one") %>% 
  mutate(variable_code = recode(variable, !!!setNames(df_questions_dap$question_code, df_questions_dap$question_name)),
         variable_label = recode(variable, !!!setNames(df_questions_dap$question_label, df_questions_dap$question_name)),
         variable_val_label = recode(variable_val, !!!choice_label_lookup))

# convert to percentage
full_analysis_long_verification <- full_analysis_labels_verification %>% 
  mutate(`mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>% 
  select(`Question code`= variable_code, 
         `Question`= variable,
         `Question label`= variable_label,
         variable, 
         `choices/options` = variable_val, 
         `choices/options label` = variable_val_label, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val,
         select_type,
         level) %>% 
  mutate(dataset = "IPE verification data")

# output analysis
write_csv(full_analysis_long_verification %>% select(-select_type), 
          paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_ipe_verification.csv"), na="")
write_csv(full_analysis_long_verification, paste0("outputs/full_analysis_lf_ipe_verification.csv"), na="")


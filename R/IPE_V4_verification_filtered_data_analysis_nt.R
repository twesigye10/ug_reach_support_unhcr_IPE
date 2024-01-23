library(tidyverse)
library(srvyr)
library(supporteR)


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
dap_verification_hh <- read_csv("inputs/r_dap_ipe_verification_filtered_hh.csv")
dap_verification_individual <- read_csv("inputs/r_dap_ipe_verification_filtered_ind.csv")


# verif filtered data
verif_filtered_data_path <- "inputs/ipe_verif_filtered_data_with_composites.xlsx"
# verif hh data
verif_filtered_data_nms <- names(readxl::read_excel(path = verif_filtered_data_path, n_max = 2000, sheet = "verif_hh_data"))
verif_filtered_c_types <- ifelse(str_detect(string = verif_filtered_data_nms, pattern = "i.dependency_ratio"), "numeric", "text")
df_verif_hh_data <- readxl::read_excel(path = verif_filtered_data_path, 
                                       sheet = "verif_hh_data", 
                                       col_types = verif_filtered_c_types, na = "NA")

# verification individual data
verif_ind_data_nms <- names(readxl::read_excel(path = verif_filtered_data_path, n_max = 2000, sheet = "verif_individual_data"))
verif_ind_c_types <- ifelse(str_detect(string = verif_ind_data_nms, pattern = "progres_size|progres_age"), "numeric", "text")
df_verif_individual_data <- readxl::read_excel(path = verif_filtered_data_path, 
                                               sheet = "verif_individual_data", 
                                               col_types = verif_ind_c_types, na = "NA") %>% 
  mutate(across(.cols = starts_with("i."), .fns = ~as.character(.x)))


# create weights ----------------------------------------------------------
# refugee weights table
ref_weight_table <- read_csv("inputs/ipe_weights_table.csv")


# verif hh level analysis -------------------------------------------------

df_verif_hh_data_with_weights <- df_verif_hh_data %>%  
  left_join(ref_weight_table, by = "strata") %>% 
  filter(!is.na(weights))

hh_svy_verification <- as_survey(.data = df_verif_hh_data_with_weights, strata = strata, weights = weights)

df_hh_level_analysis_verif <- analysistools::create_analysis(design = hh_svy_verification, loa = dap_verification_hh, sm_separator = "/")


# verif individual level analysis -----------------------------------------

df_verif_individual_data_with_weights <- df_verif_individual_data %>% 
  left_join(ref_weight_table, by = "strata") %>% 
  filter(!is.na(weights))

ind_svy_verification <- as_survey(.data = df_verif_individual_data_with_weights, strata = strata, weights = weights)

df_ind_level_analysis_verif <- analysistools::create_analysis(design = ind_svy_verification, loa = dap_verification_individual, sm_separator = "/")

# # merge analysis
# 
# combined_analysis_verification <- bind_rows(df_main_analysis_verification, df_prot_analysis)
# 
# # add labels
# full_analysis_labels_verification <- combined_analysis_verification %>% 
#   mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
#          select_type = "select_one") %>% 
#   mutate(variable_code = recode(variable, !!!setNames(df_questions_dap$question_code, df_questions_dap$question_name)),
#          variable_label = recode(variable, !!!setNames(df_questions_dap$question_label, df_questions_dap$question_name)),
#          variable_val_label = recode(variable_val, !!!choice_label_lookup))
# 
# # convert to percentage
# full_analysis_long_verification <- full_analysis_labels_verification %>% 
#   mutate(`mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
#          `mean/pct` = round(`mean/pct`, digits = 2)) %>% 
#   select(`Question code`= variable_code, 
#          `Question`= variable,
#          `Question label`= variable_label,
#          variable, 
#          `choices/options` = variable_val, 
#          `choices/options label` = variable_val_label, 
#          `Results(mean/percentage)` = `mean/pct`, 
#          n_unweighted, 
#          population, 
#          subset_1_name, 
#          subset_1_val,
#          select_type,
#          level) %>% 
#   mutate(dataset = "IPE verification data")

# output analysis
analysis_out_list <- list("HH level analysis" = df_hh_level_analysis_verif$results_table,
                          "Individual level analysis" = df_ind_level_analysis_verif$results_table)

openxlsx::write.xlsx(analysis_out_list, paste0("outputs/", butteR::date_file_prefix(), "_analysis_ipe_verification_filtered.xlsx"))
openxlsx::write.xlsx(analysis_out_list, paste0("outputs/analysis_ipe_verification_filtered.xlsx"))


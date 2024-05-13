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

# request
ipe_v4_re_path <- "support_files/IPE V4 analysis request/IPE V4 Data Analysis Request.xlsx"
df_cols_for_verif_qn_labels <- readxl::read_xlsx(path = ipe_v4_re_path, sheet = "Recoded indicator list") %>% 
  filter(`Tool/dataset` %in% c("IPE Verification tool")) %>% 
  select(indicator_label = Indicator, used_code = `used indicator name`) %>% 
  filter(!is.na(used_code))

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


# output analysis ---------------------------------------------------------

analysis_cols <- c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")

df_hh_level_analysis_verif_labels <- df_hh_level_analysis_verif$results_table %>% 
  mutate(across(.cols = any_of(analysis_cols), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .))) %>% 
  mutate(indicator = ifelse(analysis_var %in% c(df_cols_for_verif_qn_labels$used_code), recode(analysis_var, !!!setNames(df_cols_for_verif_qn_labels$indicator_label, df_cols_for_verif_qn_labels$used_code)), analysis_var),
         indicator = ifelse(analysis_var %in% c("i.hh_top3_primary_needs_2", "i.hh_top3_primary_needs_3"), "Top 3 most commonly reported primary HH needs at the time of data collection", indicator),
         result = round(ifelse(analysis_type %in% c("prop_select_one", "prop_select_multiple"), stat * 100, stat), 3),
         analysis_var_value = recode(analysis_var_value, !!!choice_label_lookup))

df_ind_level_analysis_verif_labels <- df_ind_level_analysis_verif$results_table %>% 
  mutate(across(.cols = any_of(analysis_cols), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .))) %>% 
  mutate(indicator = ifelse(analysis_var %in% c(df_cols_for_verif_qn_labels$used_code), recode(analysis_var, !!!setNames(df_cols_for_verif_qn_labels$indicator_label, df_cols_for_verif_qn_labels$used_code)), analysis_var),
         result = round(ifelse(analysis_type %in% c("prop_select_one", "prop_select_multiple"), stat * 100, stat), 3),
         analysis_var_value = recode(analysis_var_value, !!!choice_label_lookup))

analysis_out_list <- list("HH level analysis" = df_hh_level_analysis_verif_labels,
                          "Individual level analysis" = df_ind_level_analysis_verif_labels)

openxlsx::write.xlsx(analysis_out_list, paste0("outputs/", butteR::date_file_prefix(), "_analysis_ipe_verification_filtered.xlsx"))
openxlsx::write.xlsx(analysis_out_list, paste0("outputs/analysis_ipe_verification_filtered.xlsx"))


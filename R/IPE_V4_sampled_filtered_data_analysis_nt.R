library(tidyverse)
library(srvyr)
library(supporteR)
library(cleaningtools)

source("R/composite_indicators.R")
source("R/make_weights.R")

# clean data
sampled_filtered_data_path <- "inputs/ipe_hh_sampled_filtered_data_with_composites.xlsx"

filtered_data_nms <- names(readxl::read_excel(path = sampled_filtered_data_path, n_max = 2000, sheet = "sampled_hh_data"))
filtered_c_types <- ifelse(str_detect(string = filtered_data_nms, pattern = "_other$"), "text", "guess")

df_sample_filtered_with_composites <- readxl::read_excel(path = sampled_filtered_data_path, sheet = "sampled_hh_data", col_types = filtered_c_types, na = "NA") %>% 
  mutate(number_lack_of_shelter_space = as.numeric(number_lack_of_shelter_space),
         number_hh_latrine_shared_with = as.numeric(number_hh_latrine_shared_with),
         total_debt = as.numeric(total_debt),
         i.number_water_container = as.numeric(i.number_water_container),
         i.number_tarpaulin = as.numeric(i.number_tarpaulin),
         i.number_plastic_bucket = as.numeric(i.number_plastic_bucket),
         i.number_solar_lamp = as.numeric(i.number_solar_lamp),
         i.number_kitchen_set = as.numeric(i.number_kitchen_set)
  )

df_sample_mental_health_with_composites <- readxl::read_excel(path = sampled_filtered_data_path, sheet = "sampled_individual_data", na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey %>% 
  select(type, name, label) %>% 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# request
ipe_v4_re_path <- "support_files/IPE V4 analysis request/IPE V4 Data Analysis Request.xlsx"
df_cols_for_sample_qn_labels <- readxl::read_xlsx(path = ipe_v4_re_path, sheet = "Recoded indicator list") %>% 
  filter(`Tool/dataset` %in% c("IPE Sample tool", "IPE Sample data")) %>% 
  select(indicator_label = Indicator, used_code = `used indicator name`) %>% 
  filter(!is.na(used_code))

# dap
dap <- read_csv("inputs/r_dap_ipe_sampled_filtered_nt.csv")
dap_mh <- read_csv("inputs/r_dap_ipe_sample_mh_filtered.csv")

df_ref_pop <- read_csv("inputs/refugee_population_ipe.csv")


# create weights ----------------------------------------------------------

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_sample_filtered_with_composites, 
                                              input_refugee_pop = df_ref_pop)
write_csv(ref_weight_table, "outputs/ipe_weights_table.csv")
write_csv(ref_weight_table, "inputs/ipe_weights_table.csv")

df_ref_with_weights <- df_sample_filtered_with_composites %>% 
  mutate(i.number_solar_lamp = as.numeric(i.number_solar_lamp)) %>% 
  left_join(ref_weight_table, by = "strata")

# main analysis ----------------------------------------------------------------

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights)

df_main_analysis_nt <- analysistools::create_analysis(design = ref_svy, 
                                                      loa = dap %>% filter(!analysis_var %in% c("access_to_agriculture_plot_how")), 
                                                      sm_separator = "/")

# mental health -----------------------------------------------------------

df_mh_with_weights <- df_sample_mental_health_with_composites %>% 
  left_join(ref_weight_table, by = "strata")

mh_svy <- as_survey(.data = df_mh_with_weights, strata = strata, weights = weights)

df_mental_health_analysis_nt <- analysistools::create_analysis(design = mh_svy, loa = dap_mh, sm_separator = "/")

# output analysis ----------------------------------------------------------

analysis_cols <- c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")

df_main_analysis_labels <- df_main_analysis_nt$results_table %>% 
  mutate(across(.cols = any_of(analysis_cols), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .))) %>% 
  mutate(indicator = ifelse(analysis_var %in% c(df_cols_for_sample_qn_labels$used_code), recode(analysis_var, !!!setNames(df_cols_for_sample_qn_labels$indicator_label, df_cols_for_sample_qn_labels$used_code)), analysis_var),
         indicator = ifelse(analysis_var %in% c("most_important_sources_of_earnings_rank_2", "most_important_sources_of_earnings_rank_3"), "Top 3 most commonly reported sources of HH income in the past year prior to data collection", indicator),
         result = round(ifelse(analysis_type %in% c("prop_select_one", "prop_select_multiple"), stat * 100, stat), 3))
  
df_mental_health_analysis_labels <- df_mental_health_analysis_nt$results_table %>% 
  mutate(across(.cols = any_of(analysis_cols), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .))) %>% 
  mutate(indicator = ifelse(analysis_var %in% c(df_cols_for_sample_qn_labels$used_code), recode(analysis_var, !!!setNames(df_cols_for_sample_qn_labels$indicator_label, df_cols_for_sample_qn_labels$used_code)), analysis_var),
         result = round(ifelse(analysis_type %in% c("prop_select_one", "prop_select_multiple"), stat * 100, stat), 3))
  
sample_analysis_out_list <- list("HH level analysis" = df_main_analysis_labels,
                          "Individual level analysis" = df_mental_health_analysis_labels)

openxlsx::write.xlsx(sample_analysis_out_list, paste0("outputs/", butteR::date_file_prefix(), "_analysis_ipe_hh_sampled_filtered.xlsx"))
openxlsx::write.xlsx(sample_analysis_out_list, paste0("outputs/analysis_ipe_hh_sampled_filtered.xlsx"))

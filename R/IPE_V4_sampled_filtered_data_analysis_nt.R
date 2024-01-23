library(tidyverse)
library(srvyr)
library(supporteR)

source("R/composite_indicators.R")
source("R/make_weights.R")

# clean data
sampled_filtered_data_path <- "inputs/ipe_hh_sampled_filtered_data_with_composites.xlsx"

filtered_data_nms <- names(readxl::read_excel(path = sampled_filtered_data_path, n_max = 2000, sheet = "sampled_hh_data"))
filtered_c_types <- ifelse(str_detect(string = filtered_data_nms, pattern = "_other$"), "text", "guess")

df_sample_filtered_with_composites <- readxl::read_excel(path = sampled_filtered_data_path, sheet = "sampled_hh_data", col_types = filtered_c_types, na = "NA")

df_sample_mental_health_with_composites <- readxl::read_excel(path = sampled_filtered_data_path, sheet = "sampled_individual_data", na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey %>% 
  select(type, name, label) %>% 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

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

# merge analysis ----------------------------------------------------------

# combined_analysis_nt <- df_main_analysis_nt

# add labels
# full_analysis_labels <- combined_analysis %>%  
#   mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
#          int.variable = variable) %>% 
#   left_join(df_tool_data_support, by = c("int.variable" = "name")) %>% 
#   relocate(label, .after = variable) %>% 
#   mutate(select_type = case_when(int.variable %in% c("children_not_attending") ~ "integer",
#                                  int.variable %in% c("travel_time_primary", 
#                                                      "travel_time_secondary",
#                                                      "travel_time_clinic") ~ "select_one",
#                                  TRUE ~ select_type))
# 
# # convert to percentage
# full_analysis_long <- full_analysis_labels %>% 
#   mutate(label = ifelse(is.na(label), variable, label),
#          `mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
#          `mean/pct` = round(`mean/pct`, digits = 2)) %>% 
#   select(`Question`= label, 
#          variable, 
#          `choices/options` = variable_val, 
#          `Results(mean/percentage)` = `mean/pct`, 
#          n_unweighted, 
#          population, 
#          subset_1_name, 
#          subset_1_val,
#          select_type,
#          level) %>% 
#   mutate(dataset = "IPE sampled data")
# 
# # output analysis
sample_analysis_out_list <- list("HH level analysis" = df_main_analysis_nt$results_table,
                          "Individual level analysis" = df_mental_health_analysis_nt$results_table)

openxlsx::write.xlsx(sample_analysis_out_list, paste0("outputs/", butteR::date_file_prefix(), "_analysis_ipe_hh_sampled_filtered.xlsx"))
openxlsx::write.xlsx(sample_analysis_out_list, paste0("outputs/analysis_ipe_hh_sampled_filtered.xlsx"))
library(tidyverse)
library(srvyr)
library(supporteR)

source("R/composite_indicators.R")
source("R/make_weights.R")

# clean data
data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_ipe_access_services.csv")
df_ref_pop <- read_csv("inputs/refugee_population_ipe.csv")

# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators(input_df = df_main_clean_data) |>  
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
         int.variable = variable) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(label, .after = variable) |> 
  mutate(select_type = case_when(int.variable %in% c("children_not_attending") ~ "integer",
                                 int.variable %in% c("travel_time_primary", 
                                                     "travel_time_secondary",
                                                     "travel_time_clinic") ~ "select_one",
                                 TRUE ~ select_type))

# convert to percentage
full_analysis_long <- full_analysis_labels |> 
  mutate(label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  select(`Question`= label, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_ipe_hh_sampled.csv"), na="")

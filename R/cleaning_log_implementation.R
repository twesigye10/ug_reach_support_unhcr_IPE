library(tidyverse)
library(glue)
library(supporteR)

options("openxlsx.dateFormat" = "dd/mm/yyyy")

# Read data and checking log 

log_loc <- "inputs/combined_checks_IPE_questionnaire_for_sampled_households.xlsx"

log_col_types <- ifelse(str_detect(string = names(readxl::read_excel(path = log_loc, n_max = 5000)), pattern = "start_date|checked_date"), "date",  "text")

df_cleaning_log <- readxl::read_excel(log_loc, col_types = log_col_types) %>% 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) %>%
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>% 
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         # sheet = NA,
         # index = NA,
         relevant = NA) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)


# raw data
loc_data <- "inputs/Individual_Profiling_Exercise_Questionnaire_for_Sampled_Households.xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time",
                    "date_last_received")

data_nms <- names(readxl::read_excel(path = loc_data, sheet = "main_data", n_max = 5000))
c_types <- case_when(str_detect(string = data_nms, pattern = "_other$") ~ "text", 
                     # str_detect(string = data_nms, pattern = "start|end|today|starttime|endtime|_submission_time|_submission__submission_time|date_last_received") ~ "date",
                     TRUE ~ "guess")

df_raw_data <- readxl::read_excel(path = loc_data, sheet = "main_data", col_types = c_types) %>% 
  mutate(today = as_date(today),
         date_last_received = as_date(date_last_received)) %>% 
  group_by(`_uuid`) %>% 
  mutate(`_uuid` = ifelse(row_number() > 1, paste0(`_uuid`, "2"), `_uuid`)) %>% 
  ungroup()

# loop
data_nms_loop <- names(readxl::read_excel(path = loc_data, sheet = "mental_health", n_max = 5000))
c_types_loop <- ifelse(str_detect(string = data_nms_loop, pattern = "^feel_so_|help_from_mhpss_worker|often_unable_to_carry_out_essential_activities_due_to_feelings"), "text", "guess")

loop_data <- readxl::read_excel(loc_data, sheet = "mental_health", col_types = c_types_loop)

df_log_remove_loop_entry <- df_cleaning_log %>% 
  filter(type %in% c("remove_loop_entry"))

df_loop_mental_health <- loop_data %>%
  filter(!`_index` %in% df_log_remove_loop_entry$index, `_submission__uuid` %in% df_raw_data$`_uuid`)

# tool
loc_tool <- "inputs/Individual_Profiling_Exercise_Tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# main dataset ------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log %>% 
  filter(is.na(sheet))
rm(df_cleaning_log)
df_cleaning_step <- supporteR::cleaning_support(input_df_raw_data = df_raw_data,
                                               input_df_survey = df_survey,
                                               input_df_choices = df_choices,
                                               input_df_cleaning_log = df_cleaning_log_main) 

df_cleaned_data <- df_cleaning_step %>% 
  mutate(bucket_18l_num = round(18*bucket_18l_num,0),
         bucket_15l_num = round(15*bucket_15l_num,0),
         bucket_10l_num = round(10*bucket_10l_num,0),
         jerrycan_20l_num = round(20*jerrycan_20l_num,0),
         jerrycan_10l_num = round(10*jerrycan_10l_num,0),
         jerrycan_5l_num = round(5*jerrycan_5l_num,0),
         jerrycan_3l_num = round(3*jerrycan_3l_num,0),
         ) %>% 
  rowwise() %>% 
  mutate(calc_monthly_expenditure = sum(c_across(exp_savings:exp_sanitary_materials), na.rm = TRUE),
        int.calc_total_volume = sum(c_across(bucket_18l_num:jerrycan_3l_num), na.rm = TRUE),
         ) %>% 
  ungroup() %>% 
  mutate(calc_total_volume = int.calc_total_volume * number_of_trips_for_each_container,
         calc_total_volume_per_person = (calc_total_volume/(hh_size*number_of_days_collected_water_lasts))) %>% 
  mutate(across(contains("/"), .fns = ~ case_when(.x %in% c(TRUE) ~ "1",
                                                  .x %in% c(FALSE) ~ "0"))) %>% 
  mutate(date_last_received = as_date(date_last_received)) %>% 
  select(-starts_with("int."))

# write final datasets out -----------------------------------------------

list_of_clean_datasets <- list("cleaned_data" = df_cleaned_data,
                               "mental_health" = df_loop_mental_health %>% filter(`_submission__uuid` %in% df_cleaned_data$uuid))

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_ipe_hh_sampled.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_ipe_hh_sampled_no_NA.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")

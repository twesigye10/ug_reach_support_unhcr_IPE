library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# Read data and checking log 

log_loc <- "inputs/combined_checks_IPE_questionnaire_for_sampled_households.xlsx"

log_col_types <- ifelse(str_detect(string = names(readxl::read_excel(path = log_loc, n_max = 5000)), pattern = "start_date|checked_date"), "date",  "text")

df_cleaning_log <- readxl::read_excel(log_loc, col_types = log_col_types) |> 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) |>
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) |> 
  filter(!is.na(value), !is.na(uuid)) |>
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         # sheet = NA,
         # index = NA,
         relevant = NA) |>
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)


# raw data
loc_data <- "inputs/Individual_Profiling_Exercise_Questionnaire_for_Sampled_Households.xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time",
                    "date_last_received")

data_nms <- names(readxl::read_excel(path = loc_data, n_max = 5000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) |> 
  group_by(`_uuid`) |> 
  filter(row_number() == 1) |> 
  ungroup()

# final shared hh data
sampled_data_loc3 <- "inputs/Individual_Profiling_Exercise_Questionnaire_for_Sampled_Households_20230927.xlsx"
df_sampled_data3 <- readxl::read_excel(sampled_data_loc3)

# loop
loc_loop_data <- "inputs/repeat_mental_health.xlsx"

data_nms_loop <- names(readxl::read_excel(path = loc_loop_data, n_max = 5000))
c_types_loop <- ifelse(str_detect(string = data_nms_loop, pattern = "^feel_so_|help_from_mhpss_worker|often_unable_to_carry_out_essential_activities_due_to_feelings"), "text", "guess")

loop_data <- readxl::read_excel(loc_loop_data, col_types = c_types_loop)

df_loop_mental_health <- loop_data %>% 
  # filter(consent_mental_health %in% c("yes"),  !is.na(feel_so_afraid))
  filter(consent_mental_health %in% c("yes"),  if_all(feel_so_afraid:feel_so_severely_upset_about_bad_things_that_happened, ~ !is.na(.x)))

# tool
loc_tool <- "inputs/Individual_Profiling_Exercise_Tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# main dataset ------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log |> 
  filter(is.na(sheet))
rm(df_cleaning_log)
df_cleaning_step <- supporteR::cleaning_support(input_df_raw_data = df_raw_data,
                                               input_df_survey = df_survey,
                                               input_df_choices = df_choices,
                                               input_df_cleaning_log = df_cleaning_log_main) 

df_cleaned_data <- df_cleaning_step |> 
  mutate(across(.cols = -c(any_of(cols_to_escape), matches("_age$|^age_|uuid")),
                .fns = ~ifelse(str_detect(string = ., pattern = "^[9]{3,9}$"), "NA", .)),
         bucket_18l_num = 18*bucket_18l_num,
         bucket_15l_num = 15*bucket_15l_num,
         bucket_10l_num = 10*bucket_10l_num,
         jerrycan_20l_num = 20*jerrycan_20l_num,
         jerrycan_10l_num = 10*jerrycan_10l_num,
         jerrycan_5l_num = 5*jerrycan_5l_num,
         jerrycan_3l_num = 3*jerrycan_3l_num,
         ) |> 
  rowwise() |> 
  mutate(calc_monthly_expenditure = sum(c_across(exp_savings:exp_sanitary_materials), na.rm = TRUE),
        int.calc_total_volume = sum(c_across(bucket_18l_num:jerrycan_3l_num), na.rm = TRUE),
         ) |> 
  ungroup() |> 
  mutate(calc_total_volume = int.calc_total_volume * number_of_trips_for_each_container,
         calc_total_volume_per_person = (calc_total_volume/(hh_size*number_of_days_collected_water_lasts))) |> 
  select(-starts_with("int."))

# add anonymizedgroup to the original raw data
df_cleaned_data_updated <- df_cleaned_data %>% 
  left_join(df_sampled_data3 %>% select(uuid = `_uuid`, anonymizedgroup)) %>% 
  mutate(GroupAnonymized = anonymizedgroup) %>% 
  select(-anonymizedgroup) %>% 
  rename(anonymizedgroup = GroupAnonymized)

# write final datasets out -----------------------------------------------

list_of_clean_datasets <- list("cleaned_data" = df_cleaned_data_updated,
                               "mental_health" = df_loop_mental_health %>% filter(`_submission__uuid` %in% df_cleaned_data_updated$uuid))

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_ipe_hh_sampled.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

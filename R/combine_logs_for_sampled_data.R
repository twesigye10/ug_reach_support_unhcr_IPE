library(tidyverse)

# combine logs ------------------------------------------------------------

logs_loc <- "inputs/"
 
df_ipe_logs_list <- list.files(path = logs_loc, 
                               pattern = "^combined_checks_IPE_questionnaire_for_sampled_households_batch", full.names = T) |> 
  set_names()

df_combined_ipe_logs <- purrr::map_dfr(.x = df_ipe_logs_list,
                              .f = ~ readxl::read_excel(path = ., col_types = ifelse(str_detect(string = names(readxl::read_excel(path = ., n_max = 1000)), pattern = "start_date|checked_date"), "date",  "text")) |>
                                dplyr::mutate(current_value = as.character(current_value), 
                                              value = as.character(value),
                                              other_text = as.character(other_text)),
                              .id = "batch_name"
                                ) |> 
  mutate(batch_name = str_replace(string = batch_name, pattern = "inputs/combined_checks_IPE_questionnaire_for_sampled_households_", replacement = ""),
         batch_name = str_replace(string = batch_name, pattern = "\\.xlsx$", replacement = ""))

rio::export(df_combined_ipe_logs, file = "outputs/combined_ipe_logs_with_batch_name.xlsx")



# summary of some log contents --------------------------------------------

df_combined_ipe_logs |> 
  filter(!adjust_log %in% c("delete_log")) |> 
  group_by(issue_id) |> 
  summarise(count = n()) |> 
  rio::export("outputs/combined_ipe_checks_summary.xlsx")

df_extract_other_specify <- df_combined_ipe_logs |>
  filter(issue_id %in% c("other_checks"), !adjust_log %in% c("delete_log")) |> 
  group_by(name, value) |> 
  summarise(count = n()) 
  
df_extract_outliers <- df_combined_ipe_logs |>
  filter(issue_id %in% c("logic_c_outlier"), !adjust_log %in% c("delete_log")) |> 
  group_by(name) |> 
  summarise(count = n()) 
  
  
  
rio::export(x = list(other_specify = df_extract_other_specify,
                     outliers = df_extract_outliers), 
            file = "outputs/combined_ipe_checks_summary_extract.xlsx")



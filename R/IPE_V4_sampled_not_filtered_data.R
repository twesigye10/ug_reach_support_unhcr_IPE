library(tidyverse)
library(supporteR)

source("R/composite_indicators.R")

# sampled data
data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 10000, sheet = "cleaned_data"))
c_types <- case_when(str_detect(string = data_nms, pattern = "_other$") ~ "text", 
                     str_detect(string = data_nms, pattern = "_num$|_fcs$|calc_monthly_expenditure|calc_total_volume_per_person") ~ "numeric",
                     TRUE ~ "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA")

df_sampled_main_not_filtered <- df_main_clean_data

# loop
mental_health_loop <- readxl::read_excel(path = data_path, sheet = "mental_health", na = "NA") %>% 
  mutate(individual_sex = ifelse(individual_sex %in% c("male"), "Male",individual_sex))

# hh data
mental_health_to_hh_level <- mental_health_loop %>% 
  rename(uuid = "_submission__uuid") %>%
  relocate(often_unable_to_carry_out_essential_activities_due_to_feelings, .before = help_from_mhpss_worker) %>% 
  unite("int.mh_feelings", feel_so_afraid:often_unable_to_carry_out_essential_activities_due_to_feelings, sep = " : ", remove = FALSE) %>% 
  mutate(int.hh_member_mh_state = case_when(str_detect(string = int.mh_feelings, pattern = "all_of_the_time|most_of_the_time") ~ "mental_illness_yes",
                                            !str_detect(string = int.mh_feelings, pattern = "all_of_the_time|most_of_the_time") ~ "mental_illness_no")) %>% 
  group_by(uuid) %>% 
  summarise(int.hh_mh_entries = paste(int.hh_member_mh_state, collapse = " : ")) %>% 
  mutate(i.hh_mh =  case_when(str_detect(string = int.hh_mh_entries, 
                                         pattern = "mental_illness_yes") ~ "mental_illness_yes",
                              !str_detect(string = int.hh_mh_entries, 
                                         pattern = "mental_illness_yes") ~ "mental_illness_no")) %>% 
  select(-starts_with("int."))

# verification data
verif_data_path <- "inputs/combined_ipe_verif_data.csv"
df_combined_verif_data <- readr::read_csv(file =  verif_data_path, na = "NULL") 
df_verif_gender_hoh_data <- df_combined_verif_data %>% 
  select(AnonymizedGrp, progres_relationshiptofpname, progres_sexname) %>% 
  group_by(AnonymizedGrp) %>% 
  mutate(int.focal_point = ifelse(progres_relationshiptofpname %in% c("Focal Point"), "HoH", "Non HoH" )) %>% 
  filter(int.focal_point %in% c("HoH")) %>% 
  mutate(i.gender_hoh = progres_sexname) %>% 
  select(AnonymizedGrp, i.gender_hoh)

# make composite indicator ------------------------------------------------

df_with_composites <- create_composites_sampled_specific_dates(input_df = df_sampled_main_not_filtered) %>%
  left_join(df_verif_gender_hoh_data, by = c("anonymizedgroup" = "AnonymizedGrp")) %>% 
  left_join(mental_health_to_hh_level) %>% 
  mutate(i.gender_hoh = ifelse(is.na(i.gender_hoh), "Missing", i.gender_hoh)) %>% 
  mutate(strata = paste0(settlement, "_refugee"))

# individual data
df_mental_health_loop_with_composites <- mental_health_loop %>% 
  mutate(int.ind_id = paste0(individual_age, "_", individual_relationship, "_", individual_sex, "_", `_submission__uuid`)) %>% 
  group_by(int.ind_id) %>% 
  mutate(int.row_num = row_number(),
         int.relation = ifelse(int.row_num > 1 & individual_relationship %in% c("Focal Point"), "not_required", "required")) %>% 
  filter(int.relation %in% c("required")) %>% 
  ungroup() %>% 
  select(-starts_with("int.")) %>% 
  create_composites_mental_health() %>% 
  filter(`_submission__uuid` %in% df_with_composites$uuid) %>% 
  left_join(df_with_composites %>% select(uuid, settlement, location_region, strata), by = c("_submission__uuid" = "uuid"))

  

data_list <- list("sampled_hh_data" = df_with_composites,
                  "sampled_individual_data" = df_mental_health_loop_with_composites)

openxlsx::write.xlsx(x = data_list,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_ipe_hh_sampled_not_filtered_data_with_composites.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")

openxlsx::write.xlsx(x = data_list,
                     file = paste0("inputs/ipe_hh_sampled_not_filtered_data_with_composites.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")

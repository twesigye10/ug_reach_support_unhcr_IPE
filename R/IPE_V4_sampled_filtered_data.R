library(tidyverse)
library(supporteR)

source("R/composite_indicators.R")

# sampled data
data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "cleaned_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_data", col_types = c_types, na = "NA")

df_sampled_main_filtered <- df_main_clean_data %>% 
  filter(settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Palorinya", 
                           "Rhino", "Kyaka Ii", "Kyangwali", "Nakivale",
                           "Rwamwanja") & (today >= as_date("2022-03-01") & today <= as_date("2022-06-30"))|
         settlement %in% c("Lobule") & (today >= as_date("2022-02-01") & today <= as_date("2022-02-28"))|
         settlement %in% c("Kiryandongo") & (today >= as_date("2022-07-01") & today <= as_date("2022-07-31"))|
         settlement %in% c("Palabek") & (today >= as_date("2022-10-01") & today <= as_date("2022-10-31"))|
         settlement %in% c("Oruchinga") & (today >= as_date("2021-11-01") & today <= as_date("2021-11-30"))
         )

# loop
mental_health_loop <- readxl::read_excel(path = data_path, sheet = "mental_health", na = "NA")

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

df_with_composites <- create_composites_sampled_specific_dates(input_df = df_sampled_main_filtered) %>%
  left_join(df_verif_gender_hoh_data, by = c("anonymizedgroup" = "AnonymizedGrp")) %>% 
  left_join(mental_health_to_hh_level) %>% 
  mutate(i.gender_hoh = ifelse(is.na(i.gender_hoh), "Missing", i.gender_hoh)) %>% 
  mutate(strata = paste0(settlement, "_refugee"))
  

openxlsx::write.xlsx(x = df_with_composites,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_ipe_hh_sampled_filtered_data_with_composites.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

openxlsx::write.xlsx(x = df_with_composites,
                     file = paste0("inputs/ipe_hh_sampled_filtered_data_with_composites.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

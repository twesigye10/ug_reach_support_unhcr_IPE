library(tidyverse)
library(lubridate)
library(glue)


# combine verification data ----------------------------------------------------

verif_data_loc <- "inputs/REACH DataPWD/"

df_ipe_verif_data_list <- list.files(path = verif_data_loc, 
                               pattern = " IPE ", full.names = T) |> 
  set_names()

df_combined_verif_data <- purrr::map_dfr(.x = df_ipe_verif_data_list,
                                       .f = ~ readr::read_delim(file = ., delim = ";"),
                                         .id = "batch_name") |> 
  mutate(batch_name = str_replace(string = batch_name, pattern = "inputs/REACH DataPWD/", replacement = ""),
         batch_name = str_replace(string = batch_name, pattern = "\\.csv$", replacement = "")) 

# rio::export(df_combined_verif_data, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_ipe_verif_data_with_batch_name.xlsx"))

# writexl::write_xlsx(x = df_combined_verif_data, path = paste0("outputs/", butteR::date_file_prefix(), "_combined_ipe_verif_data_with_batch_name.xlsx"))

readr::write_csv(x = df_combined_verif_data, file = paste0("outputs/combined_ipe_verif_data_with_batch_name.csv"))


# extract required vars for access to services ----------------------------

df_combined_verif_data_composites <- df_combined_verif_data |> 
 dplyr::mutate(coping_less_expenditure = as.character(`64`),
         primary_needs_top3 = as.character(`92`),
         # fhhh_not_attending = gender_hoh sum_not_attending_enrolled,
         # mhhh_not_attending = gender_hoh sum_not_attending_enrolled,
         #  = why_child_not_regularly_attending,
         #  = hh_member_skills_training,
         #  = ,
         child_work_1217 = as.character(`123`),
         #  = withdrew_children_from_school,
         coping_withdraw_school = as.character(`83`),
         indv_medical_illness_3months = as.character(`31`),
         indv_medical_illness_3months_sought_asst = as.character(`32`),
         indv_difficulties_control_emotions = as.character(`30`),
         int.difficulty_walking = as.character(`14`),
         int.difficulty_lifting = as.character(`15`),
         int.difficulty_selfcare = as.character(`16`),
         int.difficulty_seeing = as.character(`26`),
         int.difficulty_hearing = as.character(`27`),
         int.difficulty_communicating = as.character(`29`),
         int.difficulty_remembering = as.character(`28`),
         int.difficulty_emotions = as.character(`30`),
         # coping_withdraw_school_fhhh = case_when(coping_withdraw_school %in% c("1817") & progres_relationshiptofpname %in% c("Focal Point"), progres_sexname %in% c("Female") ~ "withdraw_school_fhhh"), # needs harmonizing non hhead responses
         # coping_withdraw_school_mhhh = case_when(coping_withdraw_school %in% c("1817") & progres_relationshiptofpname %in% c("Focal Point"), progres_sexname %in% c("male") ~ "withdraw_school_mhhh"),
         int.disability = paste(int.difficulty_walking, int.difficulty_lifting, int.difficulty_selfcare, int.difficulty_seeing, int.difficulty_hearing, int.difficulty_communicating),
         wgss_3 = ifelse(str_detect(string = int.disability, pattern = "1706|1707"), "yes_disability", "no_disability"),
         hohh_wgss_3 = case_when(wgss_3 %in% c("yes_disability") & progres_relationshiptofpname %in% c("Focal Point") ~ "disability_focal_point",
                                   wgss_3 %in% c("yes_disability") & !progres_relationshiptofpname %in% c("Focal Point") ~ "disability_other"),
         settlement = progres_coalocationlevel2name
  ) 


write_csv(df_combined_verif_data_composites, "inputs/REACH DataPWD/combined_access_to_services_extract_ipe_verif_data_all.csv")

cols_for_analysis <- c("coping_less_expenditure",
                       "primary_needs_top3",
                       "child_work_1217",
                       "coping_withdraw_school",
                       "coping_withdraw_school_fhhh",
                       "coping_withdraw_school_mhhh",
                       "indv_medical_illness_3months",
                       "indv_medical_illness_3months_sought_asst",
                       "indv_difficulties_control_emotions",
                       "wgss_3",
                       "hohh_wgss_3",
                       "settlement")
write_csv(df_combined_verif_data_composites |> select(any_of(cols_for_analysis)), "inputs/REACH DataPWD/combined_access_to_services_extract_ipe_verif_data.csv")

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

readr::write_csv(x = df_combined_verif_data, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_ipe_verif_data_with_batch_name.csv"))



# extract required vars for access to services ----------------------------

df_combined_verif_data |> 
  rename(coping_less_expenditure = "64",
         primary_needs_top3 = "92",
         # fhhh_not_attending = gender_hoh sum_not_attending_enrolled,
         # mhhh_not_attending = gender_hoh sum_not_attending_enrolled,
         #  = why_child_not_regularly_attending,
         #  = hh_member_skills_training,
         #  = ,
         child_work_1217 = "123",
         #  = withdrew_children_from_school,
         coping_withdraw_school = "83",
         indv_medical_illness_3months = "31",
         indv_medical_illness_3months_sought_asst = "32",
         indv_difficulties_control_emotions = "30",
         int.difficulty_walking = "14",
         int.difficulty_lifting = "15",
         int.difficulty_selfcare = "16",
         int.difficulty_seeing = "26",
         int.difficulty_hearing = "27",
         int.difficulty_communicating = "29",
         int.difficulty_remembering = "28",
         int.difficulty_emotions = "30") |> 
  mutate(
         # coping_withdraw_school_fhhh = case_when(coping_withdraw_school %in% c("1817") & progres_relationshiptofpname %in% c("Focal Point"), progres_sexname %in% c("Female") ~ "withdraw_school_fhhh"), # needs harmonizing non hhead responses
         # coping_withdraw_school_mhhh = case_when(coping_withdraw_school %in% c("1817") & progres_relationshiptofpname %in% c("Focal Point"), progres_sexname %in% c("male") ~ "withdraw_school_mhhh"),
         int.disability = paste(int.difficulty_walking, int.difficulty_lifting, int.difficulty_selfcare, int.difficulty_seeing, int.difficulty_hearing, int.difficulty_communicating),
         wgss_3 = ifelse(str_detect(string = int.disability, pattern = "1706|1707"), "yes_disability", "no_disability"),
         hohh_wgss_3 = case_when(wgss_3 %in% c("yes_disability") & progres_relationshiptofpname %in% c("Focal Point") ~ "disability_focal_point",
                                   wgss_3 %in% c("yes_disability") & !progres_relationshiptofpname %in% c("Focal Point") ~ "disability_other")
  ) |> 
  # select(AnonymizedGrp, progres_maritalstatusname, progres_age,
  #        progres_relationshiptofpname, progres_coalocationlevel1name, progres_coalocationlevel2name,
  #        progres_countryoforiginidname, progres_sexname,
  #        starts_with("int.")) |> 
  write_csv("inputs/REACH DataPWD/combined_access_to_services_extract_ipe_verif_data.csv")

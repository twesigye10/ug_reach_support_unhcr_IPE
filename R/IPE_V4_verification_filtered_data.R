library(tidyverse)
library(supporteR)

source("R/composite_indicators.R")

# question/choices codes and labels
df_questions <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") %>% 
  filter(!is.na(Question), !is.na(name)) %>%
  mutate(question_code = as.character(QuestionID),
         question_label = as.character(Question),
         question_name = as.character(name)) %>% 
  select(question_code, question_label, question_name)

df_choices <- readxl::read_excel("inputs/REACH DataPWD/Questions and Responses CODES.xlsx", sheet = "Sheet1") %>% 
  mutate(choice_code = as.character(AnswerID),
         choice_label = as.character(Answer)) %>% 
  select(choice_code, choice_label)

choice_label_lookup <- setNames(object = df_choices$choice_label, nm = df_choices$choice_code)
  
# clean HH data
hh_data_path <- "inputs/clean_data_ipe_hh_sampled.xlsx"
hh_data_nms <- names(readxl::read_excel(path = hh_data_path, n_max = 2000, sheet = "cleaned_data"))
hh_c_types <- ifelse(str_detect(string = hh_data_nms, pattern = "_other$"), "text", "guess")
df_hh_data <- readxl::read_excel(path = hh_data_path, sheet = "cleaned_data", col_types = hh_c_types, na = "NA") %>% 
  filter(settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Palorinya", 
                           "Rhino", "Kyaka Ii", "Kyangwali", "Nakivale",
                           "Rwamwanja") & (today >= as_date("2022-03-01") & today <= as_date("2022-06-30"))|
         settlement %in% c("Lobule") & (today >= as_date("2022-02-01") & today <= as_date("2022-02-28"))|
         settlement %in% c("Kiryandongo") & (today >= as_date("2022-07-01") & today <= as_date("2022-07-31"))|
         settlement %in% c("Palabek") & (today >= as_date("2022-10-01") & today <= as_date("2022-10-31"))|
         settlement %in% c("Oruchinga") & (today >= as_date("2021-11-01") & today <= as_date("2021-11-30"))
  ) %>% 
  mutate(strata = paste0(settlement, "_refugee"))

# verification data
verif_data_path <- "inputs/combined_ipe_verif_data.csv"

df_verif_data <- readr::read_csv(file =  verif_data_path, na = "NULL") %>% 
  janitor::remove_empty(which = "cols")
  
df_combined_verification_data <- df_verif_data %>% 
  filter(AnonymizedGrp %in% df_hh_data$anonymizedgroup) %>% 
  rename(any_of(setNames(df_questions$question_code, df_questions$question_name))) %>%
  mutate(across(.cols = -c(progres_size, progres_age), .fns = ~as.character(.x)))

# make composite indicator ------------------------------------------------

df_verification_with_composites <- df_combined_verification_data %>%
  create_composites_verification_specific_dates() %>%
  mutate(settlement = progres_coalocationlevel2name,
         strata = paste0(settlement, "_refugee")) %>%
  janitor::remove_empty(which = "cols")

write_csv(x = df_verification_with_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_ipe_verif_filtered_data_with_composites.csv"), na="")

# calculate HH level indicators -------------------------------------------

hh_indicators_verif <- list()

# Share of head of HHs, by gender // [get gender of focal points]
df_hoh_gender <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  select(AnonymizedGrp, i.hoh_by_gender = progres_sexname)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hoh_gender")

# Share of HHs with single female head of HH
df_hh_single_female_hoh <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hoh_single_female = ifelse(progres_sexname %in% c("Female") & progres_maritalstatusname %in% c("Single", "Divorced", "Separated", "Widowed"), "yes", "no")) %>% 
  select(AnonymizedGrp, i.hoh_single_female)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_single_female_hoh")

# Share of children headed HHs
df_hoh_child <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hoh_child = ifelse(progres_age < 18, "yes", "no")) %>% 
  select(AnonymizedGrp, i.hoh_child)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hoh_child")

# Dependency Ratio
df_dependency <- df_verification_with_composites %>% 
  mutate(int.dependency_cat = case_when(progres_age < 15 ~ "dependant",
                                        progres_age <= 66 ~ "non_dependant",
                                        progres_age > 65 ~ "dependant")) %>%
  group_by(AnonymizedGrp, progres_size, int.dependency_cat) %>% 
  summarise(int.dependency_cat_count = n()) %>% 
  pivot_wider(names_from = int.dependency_cat, values_from = int.dependency_cat_count) %>% 
  mutate(int.dependency_ratio = case_when(is.na(dependant) & is.na(non_dependant) ~ NA_real_,
                                          is.na(dependant) & non_dependant > 0 ~ 0,
                                          is.na(non_dependant) & dependant > 0 ~ dependant/progres_size,
                                          non_dependant > 0 & dependant > 0 ~ dependant/non_dependant),
         i.dependency_ratio = round(int.dependency_ratio, 2)) %>% 
  ungroup() %>% 
  select(AnonymizedGrp, i.dependency_ratio)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_dependency")

# % of HHs reported having at least one of their child not living with them
df_hh_child_not_living_home <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hh_with_child_outside_of_home = case_when(child_currently_not_living_with_you %in% c(1694) ~ "yes", 
                                                     child_currently_not_living_with_you %in% c(1695) ~ "no")) %>% 
  select(AnonymizedGrp, i.hh_with_child_outside_of_home)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_child_not_living_home")

# % of HHs reported having at least one of their child not living with them by child's location at the time of data collection
df_hh_child_not_living_home_by_childlocation <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hh_with_child_outside_of_home_by_childlocation = case_when(where_children_are_living %in% c(1699) ~ "Under care of another family in Uganda (foster family)", 
                                                                      where_children_are_living %in% c(1700) ~ "Under care of another relative (kinship care arrangement) in Uganda",
                                                                      where_children_are_living %in% c(1701) ~ "Under care of another family in his/her country of origin",
                                                                      where_children_are_living %in% c(1702) ~ "Living alone independently in another location",
                                                                      where_children_are_living %in% c(1703) ~ "Living in a third country (not Uganda nor country of origin)" )) %>% 
  select(AnonymizedGrp, i.hh_with_child_outside_of_home_by_childlocation = where_children_are_living)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_child_not_living_home_by_childlocation")

 
# % of HHs with a household member with WGSS level 3 disability
df_hh_member_with_disability <- df_verification_with_composites %>%
  group_by(AnonymizedGrp) %>% 
  summarise(int.disability = paste(i.hh_member_disability_status, collapse = " : ")) %>% 
  mutate(i.hh_with_disabled_member = case_when(str_detect(string = int.disability, pattern = "yes_disability") ~ "yes_disability", 
                                               !str_detect(string = int.disability, pattern = "yes_disability") ~ "no_disability")) %>% 
  select(AnonymizedGrp, i.hh_with_disabled_member)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_member_with_disability")

# Share of head of HHs with WGSS level 3 disability
df_hoh_with_disability <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  select(AnonymizedGrp, i.hoh_disability = i.hh_member_disability_status)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hoh_with_disability")

# % of HHs reported having children working for payment
df_hh_children_working_for_payment <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hh_children_worked_forpayment = case_when(children_5_17_years_working_to_support_hh_for_payment %in% c(1694) ~ "yes",
                                                     children_5_17_years_working_to_support_hh_for_payment %in% c(1695) ~ "no")) %>% 
  select(AnonymizedGrp, i.hh_children_worked_forpayment)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_children_working_for_payment")

# Average time children worked per week for payment
df_avg_time_children_working_for_payment <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.avg_time_children_worked_forpayment = case_when(avg_time_child_working_payment %in% c(1759) ~ "Between 1 and 13 hours",
                                                           avg_time_child_working_payment %in% c(1760) ~ "Between 14 and 42 hours",
                                                           avg_time_child_working_payment %in% c(1761) ~ "more than 42",
                                                           avg_time_child_working_payment %in% c(2473) ~ "No kid working")) %>% 
  select(AnonymizedGrp, i.avg_time_children_worked_forpayment)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_avg_time_children_working_for_payment")

# % of HHs reported having children working on HH chores
df_hh_children_working_hh_chores <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hh_children_worked_Hhchores = case_when(children_supporting_household_chores %in% c(1694) ~ "yes",
                                                     children_supporting_household_chores %in% c(1695) ~ "no")) %>% 
  select(AnonymizedGrp, i.hh_children_worked_Hhchores)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_children_working_hh_chores")

# Average time children worked per week on HH chores
df_avg_time_children_working_hh_chores <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.avg_time_children_worked_HHchores = case_when(avg_time_child_working_household_chores %in% c(1759, 2488) ~ "Between 1 and 13 hours",
                                                         avg_time_child_working_household_chores %in% c(1760, 2489, 2490) ~ "Between 14 and 42 hours",
                                                         avg_time_child_working_household_chores %in% c(1761, 2491) ~ "more than 42",
                                                         avg_time_child_working_household_chores %in% c(2473, 2492) ~ "No kid working")) %>% 
  select(AnonymizedGrp, i.avg_time_children_worked_HHchores)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_avg_time_children_working_hh_chores")

# % of HHs reported having children working in dangerous conditions, by condition
df_hh_children_working_dangerous_conditions <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hh_children_dangerous_work_conditions = case_when(child_work_involve %in% c(1762) ~ "Carrying heavy loads",
                                                             child_work_involve %in% c(1763) ~ "Working with dangerous tools/operating heavy equipment",
                                                             child_work_involve %in% c(1764) ~ "Working at heights",
                                                             child_work_involve %in% c(1765) ~ "Collecting or sorting garbage",
                                                             child_work_involve %in% c(1766) ~ "Exposure to extreme heat or humidity",
                                                             child_work_involve %in% c(1767) ~ "Exposure to loud noise or vibrations",
                                                             child_work_involve %in% c(1768) ~ "Exposure to harassment",
                                                             child_work_involve %in% c(1769) ~ "Exposure to sexual and/or gender-based violence",
                                                             child_work_involve %in% c(1770) ~ "None of the above")) %>% 
  select(AnonymizedGrp, i.hh_children_dangerous_work_conditions)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_children_working_dangerous_conditions")

# % of HHs in the emergency / crisis / stress / none category of the Livelihood Coping Strategy Index
df_hh_lcsi <- df_verification_with_composites %>%
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(across(.cols = spent_savings:restricted_consumption_of_adults_for_children, .fns = ~case_when(.x %in% c("1820") ~ "1819",
                                                                                                       .x %in% c("2472") ~ NA_character_,
                                                                                                       TRUE ~ .x))) %>% 
  addindicators::add_lcsi(lcsi_stress_vars = c("borrowed_food_or_relied_on_help", "borrowed_money_to_cover_basic_needs_health_rent", "sent_hh_members_to_eat_elsewhere", "sold_more_non_productive_animals_than_usual"),
                          lcsi_crisis_vars = c("consume_seed_stock_held_for_next_season", "reduced_essential_non_food_expenditures_such_as_education_health", "withdrew_children_from_school"),
                          lcsi_emergency_vars = c("accepted_high_risk_illegal_exploitative_temporary_jobs", "sold_last_female_animals", "sent_household_members_to_beg"),
                          yes_val = "1817",
                          no_val = "1819",
                          exhausted_val = "1816",
                          not_applicable_val = "1818") %>% 
  rename(i.lcsi_cat = lcsi_cat ) %>% 
  select(AnonymizedGrp, i.lcsi_cat)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_hh_lcsi")

# Top 3 most commonly reported primary HH needs at the time of data collection
# hh_top3_primary_needs
df_verif_top_primary_needs <- df_verification_with_composites %>%
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  mutate(i.hh_top3_primary_needs_1 = recode(households_3_main_unmet_needs, !!!choice_label_lookup), i.hh_top3_primary_needs_2 = recode(households_3_main_unmet_needs_2nd, !!!choice_label_lookup), i.hh_top3_primary_needs_3 = recode(households_3_main_unmet_needs_3rd, !!!choice_label_lookup)) %>% 
  select(AnonymizedGrp, i.hh_top3_primary_needs_1, i.hh_top3_primary_needs_2, i.hh_top3_primary_needs_3)
add_checks_data_to_list(input_list_name = "hh_indicators_verif", input_df_name = "df_verif_top_primary_needs")


# combine the hh data for verification ------------------------------------

df_hh_vel_cols_support <- df_verification_with_composites %>% 
  filter(progres_relationshiptofpname %in% c("Focal Point")) %>% 
  select(AnonymizedGrp, settlement, i.gender_hoh = progres_sexname, location_region, strata)


df_combined_hh_indicators_verif <- hh_indicators_verif %>%
  reduce(.f = full_join, by = 'AnonymizedGrp') %>% 
  left_join(df_hh_vel_cols_support, by = 'AnonymizedGrp') %>% 
  mutate(i.gender_hoh = ifelse(is.na(i.gender_hoh), "Missing", i.gender_hoh))


# write_csv(x = df_combined_hh_indicators_verif, file = paste0("outputs/", butteR::date_file_prefix(), "_ipe_verif_extracted_hh_data_with_composites.csv"), na="")



# export verif data together ----------------------------------------------

verif_data_hh_and_ind <- list("verif_hh_data" = df_combined_hh_indicators_verif,
                              "verif_individual_data" = df_verification_with_composites)

openxlsx::write.xlsx(x = verif_data_hh_and_ind, file = paste0("outputs/", butteR::date_file_prefix(), 
                                         "_ipe_verif_filtered_data_with_composites.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")

openxlsx::write.xlsx(x = verif_data_hh_and_ind, file = paste0("inputs/ipe_verif_filtered_data_with_composites.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")


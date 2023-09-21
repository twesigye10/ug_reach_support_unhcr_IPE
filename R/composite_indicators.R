# creating composite indicators -------------------------------------------

create_composites_sampled <- function(input_df) {
  input_df %>% 
    mutate(int.children_not_attending = ifelse(time_primary_school %in% c("not_applicable")|time_seconday_school %in% c("not_applicable"), num_hh_age_6_12 + num_hh_age_13_16, NA_real_)
           ) %>%  
    mutate(children_not_attending  = int.children_not_attending,
           travel_time_primary  = ifelse(time_primary_school %in% c("not_applicable"), NA_character_, time_primary_school),
           travel_time_secondary  = ifelse(time_seconday_school %in% c("not_applicable"), NA_character_, time_seconday_school),
           travel_time_clinic  = time_hc,
           location_kamp_settlement = ifelse(settlement %in% c("Kampala"), "Kampala", "Settlements")
    ) %>%  
    rowwise() %>%  # land and energy analysis composites
    mutate(int.fuel_lighting = sum(exp_cooking_fuel_wood_paraffin_briquettes_etc, exp_lighting_battery_paraffin_solar_etc, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(location_region = case_when(settlement %in% c("Kampala") ~ "Kampala",
                                       settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                                       settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", "Rhino") ~ "West Nile"),
           monthly_expenditure = calc_monthly_expenditure,
           fuel_exp_ratio = exp_cooking_fuel_wood_paraffin_briquettes_etc/monthly_expenditure,
           lighting_exp_ratio = exp_lighting_battery_paraffin_solar_etc/monthly_expenditure,
           # int.fuel_lighting = exp_cooking_fuel_wood_paraffin_briquettes_etc + exp_cooking_fuel_wood_paraffin_briquettes_etc,
           fuel_lighting_exp_ratio = int.fuel_lighting/monthly_expenditure
    ) %>% # extra analysis composites
    mutate(i.fcs = (cereal_grain_root_fcs*2 + pulses_fcs*3 + vegetables_fcs*1 +  
                      fruits_fcs*1 + meat_fcs*4 + milk_products_fcs*4 + 
                      sugar_fcs*0.5 + oil_fats_fcs*0.5),
           i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                 i.fcs <= 35 ~ "Borderline",
                                 i.fcs <= 112 ~ "Acceptable"),
           i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
                                 hh_size <= 6 ~ "between_4_and_6_members",
                                 hh_size <= 9 ~ "between_7_and_9_members",
                                 hh_size >= 10 ~ "10_or_more_members"),
           i.calc_total_volume_per_person = case_when(calc_total_volume_per_person <= 19 ~ "less_than_20L_per_person_per_day",
                                                      calc_total_volume_per_person == 20 ~ "20L_per_person_per_day",
                                                      calc_total_volume_per_person > 20 ~ "more_than_20L_per_person_per_day"),
           i.sleeping_mat_num = case_when(sleeping_mat_num < 1 ~ "below_1_per_person",
                                          sleeping_mat_num == 1 ~ "1_per_person",
                                          sleeping_mat_num > 1 ~ "above_1_per_person"), 
           i.blanket_num = case_when(blanket_num < 1 ~ "below_1_per_person",
                                     blanket_num == 1 ~ "1_per_person",
                                     blanket_num > 1 ~ "above_1_per_person"),
           i.mosquito_net_num = case_when(mosquito_net_num < 1 ~ "below_1_per_person",
                                          mosquito_net_num == 1 ~ "1_per_person",
                                          mosquito_net_num > 1 ~ "above_1_per_person"),
           i.jerry_can_num = case_when(jerry_can_num <= 3 ~ "between_1_and_3_HH_size",
                                       jerry_can_num <= 6 ~ "between_4_and_6_HH_size",
                                       jerry_can_num <= 9 ~ "between_7_and_9_HH_size",
                                       jerry_can_num >= 10 ~ "10_or_more_HH_size"),
           i.tarpaulin_num = case_when(tarpaulin_num <= 3 ~ "between_1_and_3_HH_size",
                                       tarpaulin_num <= 6 ~ "between_4_and_6_HH_size",
                                       tarpaulin_num <= 9 ~ "between_7_and_9_HH_size",
                                       tarpaulin_num >= 10 ~ "10_or_more_HH_size"),
           i.plastic_bucket_num = case_when(plastic_bucket_num <= 3 ~ "between_1_and_3_HH_size",
                                            plastic_bucket_num <= 6 ~ "between_4_and_6_HH_size",
                                            plastic_bucket_num <= 9 ~ "between_7_and_9_HH_size",
                                            plastic_bucket_num >= 10 ~ "10_or_more_HH_size"),
           i.kitchen_set_num = case_when(kitchen_set_num <= 3 ~ "between_1_and_3_HH_size",
                                         kitchen_set_num <= 6 ~ "between_4_and_6_HH_size",
                                         kitchen_set_num <= 9 ~ "between_7_and_9_HH_size",
                                         kitchen_set_num >= 10 ~ "10_or_more_HH_size")) %>% 
    select(-c(starts_with("int.")))
}

create_composites_verification <- function(input_df) {
  input_df %>% 
    mutate(i.progres_relationshiptofpname = ifelse(progres_relationshiptofpname %in% c("Focal Point"), "hohh", "non_hohh"),
    # i.gender_hoh = ,
    children_engaged_child_labour = ifelse(avg_time_child_working_payment %in% c("1759", "1760", "1761"), "yes", "no"),
    progres_age_cat = case_when(progres_age < 5 ~ "age_0_4",
                                progres_age < 12 ~ "age_5_11",
                                progres_age < 18 ~ "age_12_17",
                                progres_age <= 59 ~ "age_18_59",
                                progres_age > 59 ~ "age_greater_59")
           ) %>% 
    select(-c(starts_with("int.")))
}

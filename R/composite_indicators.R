
# creating composites sampled ---------------------------------------------
# for use on original combined data
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
           i.calc_total_volume_per_person = case_when(calc_total_volume_per_person < 20 ~ "less_than_20L_per_person_per_day",
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

# for use on filtered data for specific dates
create_composites_sampled_specific_dates <- function(input_df) {
  input_df %>% 
    # mutate(int.children_not_attending = ifelse(time_primary_school %in% c("not_applicable")|time_seconday_school %in% c("not_applicable"), num_hh_age_6_12 + num_hh_age_13_16, NA_real_)
    #        ) %>%  
    # mutate(children_not_attending  = int.children_not_attending,
    #        travel_time_primary  = ifelse(time_primary_school %in% c("not_applicable"), NA_character_, time_primary_school),
    #        travel_time_secondary  = ifelse(time_seconday_school %in% c("not_applicable"), NA_character_, time_seconday_school),
    #        travel_time_clinic  = time_hc,
    #        location_kamp_settlement = ifelse(settlement %in% c("Kampala"), "Kampala", "Settlements")
    # ) %>%  
    # rowwise() %>%  # land and energy analysis composites
    # mutate(int.fuel_lighting = sum(exp_cooking_fuel_wood_paraffin_briquettes_etc, exp_lighting_battery_paraffin_solar_etc, na.rm = TRUE)) %>% 
    # ungroup() %>% 
    mutate(location_region = case_when(settlement %in% c("Kampala") ~ "Kampala",
                                       settlement %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                                       settlement %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", "Rhino") ~ "West Nile"),
           # monthly_expenditure = calc_monthly_expenditure,
           # fuel_exp_ratio = exp_cooking_fuel_wood_paraffin_briquettes_etc/monthly_expenditure,
           # lighting_exp_ratio = exp_lighting_battery_paraffin_solar_etc/monthly_expenditure,
           # # int.fuel_lighting = exp_cooking_fuel_wood_paraffin_briquettes_etc + exp_cooking_fuel_wood_paraffin_briquettes_etc,
           # fuel_lighting_exp_ratio = int.fuel_lighting/monthly_expenditure
    ) %>% # extra analysis composites
    mutate(i.fcs = (cereal_grain_root_fcs*2 + pulses_fcs*3 + vegetables_fcs*1 +  
                      fruits_fcs*1 + meat_fcs*4 + milk_products_fcs*4 + 
                      sugar_fcs*0.5 + oil_fats_fcs*0.5),
           i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                 i.fcs <= 35 ~ "Borderline",
                                 i.fcs <= 112 ~ "Acceptable"),
           # i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
           #                       hh_size <= 6 ~ "between_4_and_6_members",
           #                       hh_size <= 9 ~ "between_7_and_9_members",
           #                       hh_size >= 10 ~ "10_or_more_members"),
           i.calc_total_volume_per_person = case_when(calc_total_volume_per_person < 20 ~ "less_than_20L_per_person_per_day",
                                                      calc_total_volume_per_person == 20 ~ "20L_per_person_per_day",
                                                      calc_total_volume_per_person > 20 ~ "more_than_20L_per_person_per_day"),
           i.sleeping_mat_num = case_when(sleeping_mat_num/hh_size < 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                          sleeping_mat_num/hh_size == 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "1_per_person",
                                          sleeping_mat_num/hh_size > 1 & sleeping_mat_cond %in% c("good", "moderate") ~ "above_1_per_person"),
           i.blanket_num = case_when(blanket_num/hh_size < 1 & blanket_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                     blanket_num/hh_size == 1 & blanket_cond %in% c("good", "moderate") ~ "1_per_person",
                                     blanket_num/hh_size > 1 & blanket_cond %in% c("good", "moderate") ~ "above_1_per_person"),
           i.mosquito_net_num = case_when(mosquito_net_num/hh_size < 1 & mosquito_net_cond %in% c("good", "moderate") ~ "below_1_per_person",
                                          mosquito_net_num/hh_size == 1 & mosquito_net_cond %in% c("good", "moderate") ~ "1_per_person",
                                          mosquito_net_num/hh_size > 1 & mosquito_net_cond %in% c("good", "moderate") ~ "above_1_per_person"),
           int.hh_size_jerrycan_category = case_when(hh_size <= 3 & jerry_can_cond %in% c("good", "moderate") ~ "between_1_and_3_HH_size",
                                                   hh_size <= 6 & jerry_can_cond %in% c("good", "moderate") ~ "between_4_and_6_HH_size",
                                                   hh_size <= 9 & jerry_can_cond %in% c("good", "moderate") ~ "between_7_and_9_HH_size",
                                                   hh_size >= 10 & jerry_can_cond %in% c("good", "moderate") ~ "10_or_more_HH_size"),
           i.number_water_container = ifelse(!is.na(int.hh_size_jerrycan_category), as.numeric(jerry_can_num), NA_integer_),
           int.hh_size_tarpaulin_category = case_when(hh_size <= 3 & tarpaulin_cond %in% c("good", "moderate") ~ "between_1_and_3_HH_size",
                                                    hh_size <= 6 & tarpaulin_cond %in% c("good", "moderate") ~ "between_4_and_6_HH_size",
                                                    hh_size <= 9 & tarpaulin_cond %in% c("good", "moderate") ~ "between_7_and_9_HH_size",
                                                    hh_size >= 10 & tarpaulin_cond %in% c("good", "moderate") ~ "10_or_more_HH_size"),
           i.number_tarpaulin = ifelse(!is.na(int.hh_size_tarpaulin_category), as.numeric(tarpaulin_num), NA_integer_),
           int.hh_size_plastic_bucket_category = case_when(hh_size <= 3 & plastic_bucket_cond %in% c("good", "moderate") ~ "between_1_and_3_HH_size",
                                                         hh_size <= 6 & plastic_bucket_cond %in% c("good", "moderate") ~ "between_4_and_6_HH_size",
                                                         hh_size <= 9 & plastic_bucket_cond %in% c("good", "moderate") ~ "between_7_and_9_HH_size",
                                                         hh_size >= 10 & plastic_bucket_cond %in% c("good", "moderate") ~ "10_or_more_HH_size"),
           i.number_plastic_bucket = ifelse(!is.na(int.hh_size_plastic_bucket_category), as.numeric(plastic_bucket_num), NA_integer_),
           int.hh_size_solar_lamp_category = case_when(hh_size <= 3 & solar_lamp_cond %in%c("good", "moderate") ~ "between_1_and_3_HH_size",
                                                     hh_size <= 6 & solar_lamp_cond %in%c("good", "moderate") ~ "between_4_and_6_HH_size",
                                                     hh_size <= 9 & solar_lamp_cond %in%c("good", "moderate") ~ "between_7_and_9_HH_size",
                                                     hh_size >= 10 & solar_lamp_cond %in%c("good", "moderate") ~ "10_or_more_HH_size"),
           i.number_solar_lamp = ifelse(!is.na(int.hh_size_solar_lamp_category), as.numeric(solar_lamp_num), NA_character_),
           int.hh_size_kitchen_set_category = case_when(hh_size <= 3 & kitchen_set_cond %in% c("good", "moderate") ~ "between_1_and_3_HH_size",
                                         hh_size <= 6 & kitchen_set_cond %in% c("good", "moderate") ~ "between_4_and_6_HH_size",
                                         hh_size <= 9 & kitchen_set_cond %in% c("good", "moderate") ~ "between_7_and_9_HH_size",
                                         hh_size >= 10 & kitchen_set_cond %in% c("good", "moderate") ~ "10_or_more_HH_size"),
           i.number_kitchen_set = ifelse(!is.na(int.hh_size_kitchen_set_category), as.numeric(kitchen_set_num), NA_integer_),
           i.main_water_source = case_when(main_water_source %in% c("dug_well_unprotected", "spring_unprotected", 
                                                                       "surface_water_river_dam_lake_pond_stream_canal",
                                                                    "water_tank_where_rainwater_is_collected_unprotected") ~ "using_unimproved_waters_yes",
                                              main_water_source %in% c("bottled_water", "dug_well_protected", "public_water_tapstandpipe", "spring_protected",
                                                                       "tube_wellborehole_handpump", "water_piped_into_the_dwellingplot",
                                                                       "water_tank_where_rainwater_is_collected_protected") ~ "using_unimproved_waters_no"),
           i.latrine_type = case_when(latrine_type %in% c("covered_pit_latrine_with_a_slab", "ventilated_improved_pit_latrine", 
                                                             "ecosan_compost_toilet", "flush_toilet") ~ "using_unimproved_latrine_no",
                                         latrine_type %in% c("uncovered_pit_latrine_without_a_slab", "covered_pit_latrine_without_a_slab", 
                                                             "uncovered_pit_latrine_with_a_slab") ~ "using_unimproved_latrine_yes"),
           i.number_of_minutes_to_and_from_water_source = case_when(number_of_minutes_to_and_from_water_source < 30 ~ "less_than_30_min",
                                                                    (number_of_minutes_to_and_from_water_source <= 60) ~ "between_30_and_60_min",
                                                                    number_of_minutes_to_and_from_water_source > 60 ~ "more_than_60_min"),
           int.ind_monthly_meb_2001 = 440000/5,
           int.ind_monthly_expenditure = calc_monthly_expenditure/hh_size,
           i.hh_avg_exp_vs_meb = case_when(int.ind_monthly_expenditure < int.ind_monthly_meb_2001 ~ "monthly_expenditure_less_than_meb",
                                           int.ind_monthly_expenditure == int.ind_monthly_meb_2001 ~ "monthly_expenditure_equals_meb",
                                           int.ind_monthly_expenditure > int.ind_monthly_meb_2001 ~ "monthly_expenditure_greater_than_meb")
           ) %>% 
    select(-c(starts_with("int.")))
}

# composites for mental health

create_composites_mental_health <- function(input_df) {
  input_df %>%
    relocate(often_unable_to_carry_out_essential_activities_due_to_feelings, .before = help_from_mhpss_worker) %>% 
    unite("int.mh_feelings", feel_so_afraid:often_unable_to_carry_out_essential_activities_due_to_feelings, sep = " : ", remove = FALSE) %>% 
    mutate(i.mental_health_age_category = case_when(individual_age < 18 ~ "between_12_and_17_years",
                                                    individual_age < 26 ~ "between_18_and_25_years",
                                                    individual_age < 60 ~ "between_26_and_59_years",
                                                    individual_age > 59 ~ "greater_than_59_years"),
           i.hh_member_mh_state = case_when(str_detect(string = int.mh_feelings, pattern = "all_of_the_time|most_of_the_time") ~ "mental_illness_yes",
                                                       !str_detect(string = int.mh_feelings, pattern = "all_of_the_time|most_of_the_time") ~ "mental_illness_no")
    ) %>%
    
    select(-c(starts_with("int.")))
}

# creating composites verification ----------------------------------------
# for use on original combined data
create_composites_verification <- function(input_df) {
  input_df %>% 
    mutate(children_engaged_child_labour = ifelse(avg_time_child_working_payment %in% c("1759", "1760", "1761"), "yes", "no"),
           progres_age_cat = case_when(progres_age < 5 ~ "age_0_4",
                                       progres_age < 12 ~ "age_5_11",
                                       progres_age < 18 ~ "age_12_17",
                                       progres_age <= 59 ~ "age_18_59",
                                       progres_age > 59 ~ "age_greater_59")
    ) %>% 
    select(-c(starts_with("int.")))
}

# for use on filtered data based on date of data collection
create_composites_verification_specific_dates <- function(input_df) {
  input_df %>% 
    mutate(location_region = case_when(progres_coalocationlevel2name %in% c("Kampala") ~ "Kampala",
                                       progres_coalocationlevel2name %in% c("Kyaka Ii", "Kyangwali", "Nakivale", "Oruchinga", "Rwamwanja") ~ "South West",
                                       progres_coalocationlevel2name %in% c("Adjumani", "Bidibidi", "Imvepi", "Kiryandongo", "Lobule", "Palabek", "Palorinya", "Rhino") ~ "West Nile"),
           children_engaged_child_labour = ifelse(avg_time_child_working_payment %in% c("1759", "1760", "1761"), "yes", "no"),
           progres_age_cat = case_when(progres_age < 5 ~ "age_0_4",
                                       progres_age < 12 ~ "age_5_11",
                                       progres_age < 18 ~ "age_12_17",
                                       progres_age < 60 ~ "age_18_59",
                                       progres_age >= 60 ~ "age_60+"),
           i.children_not_living_with_parents = case_when(progres_age < 18 & biological_parents_living_in_household %in% c(1695) & people_living_in_home_your_relatives %in% c(1694) ~ "yes",
                                                          progres_age < 18 & biological_parents_living_in_household %in% c(1695) & people_living_in_home_your_relatives %in% c(1695) ~ "no"),
           i.disability_age_group = case_when(progres_age < 5 ~ NA_character_,
                                       progres_age < 13 ~ "age_5_12",
                                       progres_age < 19 ~ "age_13_18",
                                       progres_age < 25 ~ "age_19_24",
                                       progres_age < 60 ~ "age_25_59",
                                       progres_age >= 60 ~ "age_60+"),
           i.hh_member_disability_status =  case_when(if_any(.cols = c(difficulty_seeing,
                                                                difficulty_hearing,
                                                                difficulty_walking,
                                                                difficulty_remembering,
                                                                difficulty_selfcare,
                                                                difficulty_communicating), .fns = ~ .x %in% c(1706 , 1707) & !is.na(i.disability_age_group)) ~ "yes_disability",
                                               if_any(.cols = c(difficulty_seeing,
                                                                difficulty_hearing,
                                                                difficulty_walking,
                                                                difficulty_remembering,
                                                                difficulty_selfcare,
                                                                difficulty_communicating), .fns = ~ !.x %in% c(1706 , 1707) & !is.na(i.disability_age_group)) ~ "no_disability"),
           i.chronic_condition_age_cat = case_when(progres_age < 3 ~ "age_0_2",
                                                   progres_age < 6 ~ "age_3_5",
                                                   progres_age < 13 ~ "age_6_12",
                                                   progres_age < 19 ~ "age_13_18",
                                                   progres_age < 25 ~ "age_19_24",
                                                   progres_age < 60 ~ "age_25_59",
                                                   progres_age >= 60 ~ "age_60+"),
           i.hh_member_with_chronic_condition = case_when(medical_condition_lasted_3_months %in% c(1694) ~ "yes",
                                                          medical_condition_lasted_3_months %in% c(1695) ~ "no"),
           i.hh_member_with_chronic_condition_access_healthcare = case_when(medical_condition_lasted_3_months %in% c(1694) & been_to_hospital_for_chronic_medical %in% c(1708) ~ "yes",
                                                          medical_condition_lasted_3_months %in% c(1694) & been_to_hospital_for_chronic_medical %in% c(1709) ~ "no"),
           i.occupation_age_cat = case_when(progres_age < 1 ~ NA_character_,
                                            progres_age < 5 ~ "age_1_4",
                                            progres_age < 12 ~ "age_5_11",
                                            progres_age < 18 ~ "age_12_17",
                                            progres_age < 26 ~ "age_18_25",
                                            progres_age < 60 ~ "age_26_59",
                                            progres_age >= 60 ~ "age_60+"),
           i.hh_member_occupation_pastyear = ifelse(!is.na(i.occupation_age_cat), main_occupation_past_year, NA_character_),
           i.hh_member_worked_past7days = case_when(!is.na(i.occupation_age_cat) & worked_in_past_7_days == 1725 ~ "Worked at least one hour for pay or profit",
                                                    !is.na(i.occupation_age_cat) & worked_in_past_7_days == 1726 ~ "Did not work, but was actively searching for work",
                                                    !is.na(i.occupation_age_cat) & worked_in_past_7_days == 1727 ~ "Did not work, and was not actively searching for work"),
           i.attending_school_age_cat = case_when(progres_age < 3 ~ NA_character_,
                                            progres_age < 6 ~ "age_3_5",
                                            progres_age < 13 ~ "age_6_12",
                                            progres_age < 19 ~ "age_13_18",
                                            progres_age < 25 ~ "age_19_24",
                                            progres_age >= 25 ~ NA_character_),
           i.children_attending_school = ifelse(!is.na(i.attending_school_age_cat), attending_school_now, NA_character_)
           
           
    ) %>% 
    select(-c(starts_with("int.")))
}

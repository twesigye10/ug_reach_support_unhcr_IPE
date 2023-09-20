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
    rowwise() %>%  
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
    ) %>% 
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

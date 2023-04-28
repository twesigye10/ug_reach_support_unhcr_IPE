# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df %>% 
    mutate(int.children_not_attending = ifelse(time_primary_school %in% c("not_applicable")|time_seconday_school %in% c("not_applicable"), num_hh_age_6_12 + num_hh_age_13_16, NA_real_),
           # int.indv_12over_need_hhpss_support = help_from_mhpss_worker %in% c("yes_i_needed_help_but_did_not_get_it", "yes_i_needed_that_help_i_got_it"),
           # int.indv_12over_need_hhpss_support_not_receive = help_from_mhpss_worker %in% c("yes_i_needed_help_but_did_not_get_it"),
           ) |> 
    mutate(children_not_attending  = int.children_not_attending,
           travel_time_primary  = ifelse(time_primary_school %in% c("not_applicable"), NA_character_, time_primary_school),
           travel_time_secondary  = ifelse(time_seconday_school %in% c("not_applicable"), NA_character_, time_seconday_school),
           # exp_education_related_expenditures  = exp_education_related_expenditures,
           # exp_health_related_expenditures  = exp_health_related_expenditures,
           travel_time_clinic  = time_hc,
           # member_fell_sick  = member_fell_sick,
           # treatment_sought  = treatment_sought,
           # reason_no_treatment  = reason_no_treatment,
           # indv_12over_need_hhpss_support  = int.indv_12over_need_hhpss_support,
           # indv_12over_need_hhpss_support_not_receive  = int.indv_12over_need_hhpss_support_not_receive,
           # exp_water_for_hh_use  = exp_water_for_hh_use,
           # exp_drinking_water  = exp_drinking_water,
           # main_water_source  = main_water_source,
           # number_of_minutes_to_and_from_water_source  = number_of_minutes_to_and_from_water_source,
           # soap_available  = soap_available,
           # bathing_shelter  = bathing_shelter,
           # access_household_latrine  = access_household_latrine,
           # latrine_shared_with_other_hh  = latrine_shared_with_other_hh,
           # latrine_condition  = latrine_condition
    ) |> 
    select(-c(starts_with("int.")))
}
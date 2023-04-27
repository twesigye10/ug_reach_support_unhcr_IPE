# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df %>% 
    mutate(int.children_not_attending = ifelse(time_primary_school %in% c("not_applicable")|time_seconday_school %in% c("not_applicable"), num_hh_age_6_12 + num_hh_age_13_16, NA_real_),
           int.indv_12over_need_hhpss_support = help_from_mhpss_worker %in% c("yes_i_needed_help_but_did_not_get_it", "yes_i_needed_that_help_i_got_it"),
           int.indv_12over_need_hhpss_support_not_receive = help_from_mhpss_worker %in% c("yes_i_needed_help_but_did_not_get_it"),
           ) |> 
    mutate(i.children_not_attending  = int.children_not_attending,
           i.travel_time_primary  = time_primary_school,
           i.travel_time_secondary  = time_seconday_school,
           i.exp_education_related_expenditures  = exp_education_related_expenditures,
           i.exp_health_related_expenditures  = exp_health_related_expenditures,
           i.travel_time_clinic  = time_hc,
           i.member_fell_sick  = member_fell_sick,
           i.treatment_sought  = treatment_sought,
           i.reason_no_treatment  = reason_no_treatment,
           i.indv_12over_need_hhpss_support  = int.indv_12over_need_hhpss_support,
           i.indv_12over_need_hhpss_support_not_receive  = int.indv_12over_need_hhpss_support_not_receive,
           i.exp_water_for_hh_use  = exp_water_for_hh_use,
           i.exp_drinking_water  = exp_drinking_water,
           i.main_water_source  = main_water_source,
           i.number_of_minutes_to_and_from_water_source  = number_of_minutes_to_and_from_water_source,
           i.soap_available  = soap_available,
           i.bathing_shelter  = bathing_shelter,
           i.access_household_latrine  = access_household_latrine,
           i.latrine_shared_with_other_hh  = latrine_shared_with_other_hh,
           i.latrine_condition  = latrine_condition
    ) |> 
    select(-c(starts_with("int.")))
}
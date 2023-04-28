# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df %>% 
    mutate(int.children_not_attending = ifelse(time_primary_school %in% c("not_applicable")|time_seconday_school %in% c("not_applicable"), num_hh_age_6_12 + num_hh_age_13_16, NA_real_)
           ) |> 
    mutate(children_not_attending  = int.children_not_attending,
           travel_time_primary  = ifelse(time_primary_school %in% c("not_applicable"), NA_character_, time_primary_school),
           travel_time_secondary  = ifelse(time_seconday_school %in% c("not_applicable"), NA_character_, time_seconday_school),
           travel_time_clinic  = time_hc
    ) |> 
    select(-c(starts_with("int.")))
}
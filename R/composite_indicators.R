# creating composite indicators -------------------------------------------

create_composite_indicators_a2s <- function(input_df) {
  input_df %>% 
    mutate(int.children_not_attending = ifelse(time_primary_school %in% c("not_applicable")|time_seconday_school %in% c("not_applicable"), num_hh_age_6_12 + num_hh_age_13_16, NA_real_)
           ) |> 
    mutate(children_not_attending  = int.children_not_attending,
           travel_time_primary  = ifelse(time_primary_school %in% c("not_applicable"), NA_character_, time_primary_school),
           travel_time_secondary  = ifelse(time_seconday_school %in% c("not_applicable"), NA_character_, time_seconday_school),
           travel_time_clinic  = time_hc,
           location_kamp_settlement = ifelse(settlement %in% c("Kampala"), "Kampala", "Settlements")
    ) |> 
    select(-c(starts_with("int.")))
}

create_composites_verification_sev <- function(input_df) {
  input_df %>% 
    mutate(i.progres_relationshiptofpname = ifelse(progres_relationshiptofpname %in% c("Focal Point"), "hohh", "non_hohh"),
    # i.gender_hoh = 
           ) |> 
    select(-c(starts_with("int.")))
}
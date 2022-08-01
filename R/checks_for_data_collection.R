library(tidyverse)
library(lubridate)
library(glue)
library(sf)

source("R/support_functions.R")

# read data 
data_file <- "inputs/Individual_Profiling_Exercise_Questionnaire_for_Sampled_Households.xlsx"

data_nms <- names(readxl::read_excel(path = data_file, sheet = "Individual Profiling Exercis_", n_max = 200))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_tool_data <- readxl::read_excel(path = data_file, sheet = "Individual Profiling Exercis_", col_types = c_types) %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.settlement = ifelse(settlement == "Kyaka Ii", "Kyaka II", settlement),
         start = as_datetime(start),
         end = as_datetime(end),
         across(starts_with("calc_"), .fns = ~as.numeric(.)),
         settlement = ifelse(settlement == "Kyaka Ii", "Kyaka II", settlement))

df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "choices")

# settlement layer
thresh_dist <- 150
units(thresh_dist) <- "m"

df_settlement_layer <- sf::st_read("inputs/settlement_layer.gpkg", quiet = TRUE) %>% 
  # sf::st_transform(crs = 32636 ) %>% 
  sf::st_buffer(dist = thresh_dist)

# output holder -----------------------------------------------------------

logic_output <- list()


# no consent --------------------------------------------------------------

df_no_consent <- df_tool_data %>% 
  filter(consent == "no") %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "consent",
         i.check.current_value = consent,
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_no_consent",
         i.check.issue = "no_consent",
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent")

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_duplicate_uuid")

# duration of the survey --------------------------------------------------

min_time_of_survey <- 16
max_time_of_survey <- 180

df_c_survey_time <-  df_tool_data %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval),
         
         i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = case_when(
           int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
           int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
           TRUE ~ "normal_survey_time" ),
         i.check.issue = glue("{int.survey_time_interval} min taken to do the survey"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")%>% 
  filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time")) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_survey_time")

# availability of gps coordinates -----------------------------------------

df_c_survey_gps <-  df_tool_data %>% 
  filter(is.na(gps_coordinates)) %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval),
         
         i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "no_gps_coordinates",
         i.check.issue = glue("no gps coordinates"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")%>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_survey_gps")


# check outliers ----------------------------------------------------------

# values that lie outside the interval formed by the 2.5 and 97.5 percentiles are considered potential outliers
df_c_outliers_monthly_expenditure <- check_outliers(input_tool_data = df_tool_data,
                                                    input_column = "calc_monthly_expenditure", 
                                                    input_lower_limit = quantile(df_tool_data$calc_monthly_expenditure, 0.025, na.rm = TRUE),
                                                    input_upper_limit = quantile(df_tool_data$calc_monthly_expenditure, 0.975, na.rm = TRUE))

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_outliers_monthly_expenditure")

# check if gps points are within settlement -------------------------------

df_tool_data_pts <- df_tool_data %>% 
  mutate(i.check.settlement = ifelse(i.check.settlement == "Adjumani", zone, i.check.settlement),
         i.check.settlement = ifelse(i.check.settlement == "Maaji II", "Maaji", i.check.settlement)) %>% 
  filter(!is.na(gps_coordinates), !is.na(i.check.settlement)) %>% 
  sf::st_as_sf(coords = c("_gps_coordinates_longitude","_gps_coordinates_latitude"), crs = 4326) %>% 
  sf::st_transform(crs = 32636 ) %>% 
  select(i.check.uuid, i.check.start_date, i.check.settlement, int.check.zone = zone, int.check.village = village )

# st_write(df_tool_data_pts, "outputs/ipe_tool_data.gpkg", append = FALSE)

df_pts_out_of_settlement <- sf::st_join(df_tool_data_pts, df_settlement_layer) %>% 
  filter(is.na(Settlement_Name))

# improving the workflow for several settlements
settlements_in_data <- df_pts_out_of_settlement %>%
  filter(!i.check.settlement %in% c("NULL")) %>% 
  pull(i.check.settlement) %>%
  unique()

df_c_survey_gps_pt_not_in_settlement <- tibble()

if(length(settlements_in_data) > 0){

  for (settln in settlements_in_data) {
    # settlement polygon
    current_settl_polygon <- df_settlement_layer %>% 
      filter(Settlement_Name == settln)
    print(paste0("length of settlement ", settln, " : ", nrow(current_settl_polygon)))
    # get data for the settlement
    current_settlement_data <- df_pts_out_of_settlement %>%
      filter(i.check.settlement == settln)
    print(paste0("length of ", settln, " data: ", nrow(current_settlement_data)))
    # get distance to shapefile
    current_settlement_dist_data <- current_settlement_data %>%
      st_distance(current_settl_polygon)
    print(paste0("length of ", settln, " distance data: ", nrow(current_settlement_dist_data)))
    # join the data and distance
    format_current_settlement_dist_data <- current_settlement_data %>%
      st_drop_geometry() %>%
      mutate(int.distance_to_settlement = round(x = current_settlement_dist_data, digits = 0) + thresh_dist,
             i.check.type = "remove_survey",
             i.check.name = "",
             i.check.current_value = "",
             i.check.value = "",
             i.check.issue_id = "point_out_of_settlement_boundary",
             i.check.issue = glue("point is {int.distance_to_settlement}m from the settlement"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "",
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.so_sm_choices = "") %>%
      dplyr::select(starts_with("i.check"))%>%
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

    df_c_survey_gps_pt_not_in_settlement <- bind_rows(df_c_survey_gps_pt_not_in_settlement, format_current_settlement_dist_data)
  }
}

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_survey_gps_pt_not_in_settlement")


# logical checks ----------------------------------------------------------

# If "hh_size" = 1 and response to relation to household head "relation_to_hoh" is not "head_of_household"
df_relation_to_hoh <- df_tool_data %>% 
  filter(!relation_to_hoh %in% c("head_of_household") , hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "relation_to_hoh",
         i.check.current_value = relation_to_hoh,
         i.check.value = "",
         i.check.issue_id = "logic_c_relation_to_hoh_mismatch",
         i.check.issue = glue("relation_to_hoh: {relation_to_hoh}, but hh_size is: {hh_size}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Response to change to 'head_of_household' since respondent lives alone", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_relation_to_hoh")

# If response to "live_in_house" is no, survey needs to be checked
df_live_in_house <- df_tool_data %>% 
  filter(live_in_house == "no") %>%
  mutate(i.check.type = "remove_survey",
         i.check.name = "live_in_house",
         i.check.current_value = live_in_house,
         i.check.value = "",
         i.check.issue_id = "logic_c_respondent_not_living_in_house",
         i.check.issue = "respondent does not live in the house, but has answered for the household",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "care to be taken in deciding how to use this data", 
         i.check.reviewed = "",
         i.check.adjust_log = ""
  ) %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house")

# If hh_size = 1 and respondent does not live in the house i.e. live_in_house = no, survey needs to be checked
df_live_in_house_and_hh_size <- df_tool_data %>% 
  filter(live_in_house %in% c("no") , hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "live_in_house",
         i.check.current_value = live_in_house,
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_size_and_live_in_house_mismatch",
         i.check.issue = glue("hh_size: {hh_size}, but respondent does not live in the house i.e. live_in_house: {live_in_house}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_live_in_house_and_hh_size")

# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# food_aid_assistance = ‘no’, survey should be checked
df_food_aid_assistance <- df_tool_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), food_aid_assistance == "no") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "food_aid_assistance",
         i.check.current_value = food_aid_assistance,
         i.check.value = "",
         i.check.issue_id = "logic_c_long_live_in_house_but_no_food_aid_assistance",
         i.check.issue = glue("long_live_in_house: {long_live_in_house}, but food_aid_assistance: {food_aid_assistance}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = ", needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_food_aid_assistance")

# If respondent answered to "How long have you lived in the house?" i.e  long_live_in_house = ‘more_than_two_years’ or ‘one_to_two_years’ and 
# receive_nfi = ‘no_i_have_never_received_nfis’, survey should be checked
df_receive_nfi <- df_tool_data %>% 
  filter(long_live_in_house %in% c("more_than_two_years", "one_to_two_years"), receive_nfi == "no_i_have_never_received_nfis") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "receive_nfi",
         i.check.current_value = receive_nfi,
         i.check.value = "",
         i.check.issue_id = "logic_c_long_live_in_house_but_not_receive_nfi",
         i.check.issue = glue("long_live_in_house: {long_live_in_house}, but receive_nfi: {receive_nfi}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "needs confirmation from the field", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_receive_nfi")

# If condiments = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_condiments_fcs <- df_tool_data %>% 
  filter(condiments_fcs == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "condiments_fcs",
         i.check.current_value = as.character(condiments_fcs),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_no_eating_condiments",
         i.check.issue = glue("condiments_fcs: {condiments_fcs}, it's unlikely that a household will spend 7 days eating food without salt"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "blank variable, most enumerators misinterpreted question", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_condiments_fcs")

# If respondent answered to "What is your current main water source for drinking/cooking?" i.e  main_water_source = water_piped_into_the_dwellingplot and 
# walking_dist_drinking_water_source = 'btn_201_and_500m' or 'btn_501m_and_1km' or 'greater_than_1km', survey should be checked
df_walking_dist_drinking_water_source <- df_tool_data %>% 
  filter(walking_dist_drinking_water_source %in% c("btn_201_and_500m", "btn_501m_and_1km", "greater_than_1km"), 
         main_water_source == "water_piped_into_the_dwellingplot") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "walking_dist_drinking_water_source",
         i.check.current_value = walking_dist_drinking_water_source,
         i.check.value = "",
         i.check.issue_id = "logic_c_walking_dist_drinking_water_source",
         i.check.issue = glue("main_water_source: {main_water_source}, but walking_dist_drinking_water_source: {walking_dist_drinking_water_source}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_walking_dist_drinking_water_source")

# If respondent answered to "How many trips did you make using the containers?" i.e. number_of_trips_for_each_container > '0' 
# and total amount of water collected i.e. calc_total_volume = '0', survey should be checked
df_calc_total_volume <- df_tool_data %>% 
  filter(calc_total_volume == 0 , number_of_trips_for_each_container > 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "calc_total_volume",
         i.check.current_value = as.character(calc_total_volume),
         i.check.value = "",
         i.check.issue_id = "logic_c_water_amount_collected_and_trips_mismatch",
         i.check.issue = glue("calc_total_volume: {calc_total_volume}, but number_of_trips_for_each_container: {number_of_trips_for_each_container}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_calc_total_volume")

# If most_important_sources_of_earnings = 'none' and process_and_sell_any_agricultural_by_products or own_a_trading_business or 
# own_a_professional_office or  drive_a_household_owned_taxi_bodaboda or own_a_bar_or_restaurant or 
# own_any_other_non_agricultural_business = 'yes', survey should be checked
df_most_important_sources_of_earnings <- df_tool_data %>% 
  filter((process_and_sell_any_agricultural_by_products %in% c("yes") | own_a_trading_business %in% c("yes") | 
            own_a_professional_office %in% c("yes") | drive_a_household_owned_taxi_bodaboda %in% c("yes") |  own_a_bar_or_restaurant %in% c("yes")|
            own_any_other_non_agricultural_business %in% c("yes")), most_important_sources_of_earnings == "none") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "most_important_sources_of_earnings",
         i.check.current_value = most_important_sources_of_earnings,
         i.check.value = "",
         i.check.issue_id = "logic_c_most_important_sources_of_earnings",
         i.check.issue = "Household member has an economic activity yet source of earning is none",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_most_important_sources_of_earnings")

# If pulses_fcs = 0 i.e. household has not eaten beans in the past seven days, surveys should be checked
df_pulses_fcs <- df_tool_data %>% 
  filter(pulses_fcs == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "pulses_fcs",
         i.check.current_value = as.character(pulses_fcs),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_not_eating_pulses",
         i.check.issue = "It's unlikely that hh members will spend 7 days without eating any beans/peas/sim sim/g.nuts etc.",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_pulses_fcs")


# combine checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)

# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_others_data) %>% 
  mutate(int.name = ifelse(str_detect(string = name, pattern = "_rank_.*"), str_replace(string = name, pattern = "_rank_.*", replacement = ""), name)) %>% 
  left_join(df_survey %>% select(name, label), by = c("int.name" = "name")) %>% 
  select(-int.name) %>% 
  relocate(label, .after = name)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_", "combined_checks_IPE_questionnaire_for_sampled_households.csv"), na = "")

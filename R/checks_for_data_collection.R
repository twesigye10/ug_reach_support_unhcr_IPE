library(tidyverse)
library(lubridate)
library(glue)
library(sf)

source("R/support_functions.R")

# read data 
df_tool_data <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Questionnaire_for_Sampled_Households.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.settlement = settlement,
         start = as_datetime(start),
         end = as_datetime(end))

df_survey <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Individual_Profiling_Exercise_Tool.xlsx", sheet = "choices")

# settlement layer
thresh_dist <- 150
units(thresh_dist) <- "m"

df_settlement_layer <- sf::st_read("inputs/settlement_layer.gpkg", quiet = TRUE) %>% 
  sf::st_transform(crs = 32636 ) %>% 
  sf::st_buffer(dist = thresh_dist)

# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_duplicate_uuid")

# duration of the survey --------------------------------------------------

min_time_of_survey <- 20
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

# check if gps points are within settlement -------------------------------

df_tool_data_pts <- df_tool_data %>% 
  mutate(i.check.settlement = ifelse(i.check.settlement == "Adjumani", zone, i.check.settlement)) %>% 
  filter(!is.na(gps_coordinates), !is.na(i.check.settlement)) %>% 
  sf::st_as_sf(coords = c("_gps_coordinates_longitude","_gps_coordinates_latitude"), crs = 4326) %>% 
  sf::st_transform(crs = 32636 ) %>% 
  select(i.check.uuid, i.check.start_date, i.check.settlement, int.check.zone = zone, int.check.village = village )

# st_write(df_tool_data_pts, "outputs/ipe_tool_data.gpkg", append = FALSE)

df_pts_out_of_settlement <- sf::st_join(df_tool_data_pts, df_settlement_layer) %>% 
  filter(is.na(Settlement_Name))

# improving the workflow for several settlements
settlements_in_data <- df_pts_out_of_settlement %>%
  pull(i.check.settlement) %>%
  unique()

df_c_survey_gps_pt_not_in_settlement <- tibble()

if(length(settlements_in_data) > 0){

  for (settln in settlements_in_data) {
    # settlement polygon
    current_settl_polygon <- df_settlement_layer %>% 
      filter(Settlement_Name == settln)
    # print(paste0("length of settlement ", settln, " : ", nrow(current_settl_polygon)))
    # get data for the settlement
    current_settlement_data <- df_pts_out_of_settlement %>%
      filter(i.check.settlement == settln)
    # print(paste0("length of", settln, " data: ", nrow(current_settlement_data)))
    # get distance to shapefile
    current_settlement_dist_data <- current_settlement_data %>%
      st_distance(current_settl_polygon)
    # print(paste0("length of", settln, " distance data: ", nrow(current_settlement_dist_data)))
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

# check possibility of duplicates

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

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

checks_output <- list()


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

if(exists("df_c_survey_time")){
  if(nrow(df_c_survey_time) > 0){
    checks_output$df_c_survey_time <- df_c_survey_time
  }
}


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

if(exists("df_c_survey_gps")){
  if(nrow(df_c_survey_gps) > 0){
    checks_output$df_c_survey_gps <- df_c_survey_gps
  }
}

# check if gps points are within settlement -------------------------------

df_tool_data_pts <- df_tool_data %>% 
  filter(!is.na(gps_coordinates)) %>% 
  sf::st_as_sf(coords = c("_gps_coordinates_longitude","_gps_coordinates_latitude"), crs = 4326) %>% 
  sf::st_transform(crs = 32636 ) %>% 
  select(i.check.uuid, i.check.settlement, int.check.zone = zone, int.check.village = village )

st_write(df_tool_data_pts, "outputs/ipe_tool_data.gpkg", append = FALSE)

df_pts_out_of_settlement <- sf::st_join(df_tool_data_pts, df_settlement_layer) %>% 
  filter(is.na(Settlement_Name))

df_dist_to_settlement <- df_pts_out_of_settlement %>% 
  st_distance(df_settlement_layer %>% filter(Settlement_Name == df_pts_out_of_settlement %>% pull(i.check.settlement) %>% unique()))

df_c_survey_gps_pt_not_in_settlement <- df_pts_out_of_settlement %>% 
  st_drop_geometry() %>% 
  mutate(int.distance_to_settlement = round(x = df_dist_to_settlement, digits = 0) + thresh_dist,
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

if(exists("df_c_survey_gps_not_in_settlement")){
  if(nrow(df_c_survey_gps_pt_not_in_settlement) > 0){
    checks_output$df_c_survey_gps_pt_not_in_settlement <- df_c_survey_gps_pt_not_in_settlement
  }
}

# check possibility of duplicates


# combine checks ----------------------------------------------------------

df_logic_checks <- bind_rows(checks_output)

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

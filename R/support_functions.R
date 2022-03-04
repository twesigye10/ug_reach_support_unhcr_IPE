# function: check indicators for the dap --------------------------------------------------------
get_indicators_for_dap <- function(input_tool_name) {
  df_survey <- readxl::read_excel(paste0("inputs/", input_tool_name, ".xlsx"), sheet = "survey")
  df_survey %>% 
    filter(str_detect(string = type, pattern = "integer|select_one|select_multiple")) %>% 
    pull(name)
}

create_dap_skeleton <- function(input_tool_name, input_ignore_columns) {
  df_survey <- readxl::read_excel(paste0("inputs/", input_tool_name, ".xlsx"), sheet = "survey")
  df_survey %>% 
    filter(str_detect(string = type, pattern = "integer|calculate|select_one|select_multiple"), !name %in% input_ignore_columns) %>% 
    select(type, variable = name, label) %>% 
    mutate(level = "NA",
           split = "all",
           disaggregation = "NA",
           subset_1 = "NA") %>% 
    write_csv(file = paste0("outputs/r_dap_", str_replace(string = input_tool_name, pattern = fixed(pattern = "_tool", ignore_case = TRUE), replacement = ""), ".csv"), na = "NA")
}

# duplicate uuid ----------------------------------------------------------

# check uuids of surveys for duplicates
check_duplicates_by_uuid <- function(input_tool_data) {
  input_tool_data %>% 
    group_by(i.check.uuid) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank > 1) %>%  
    mutate(
      i.check.type = "remove_survey",
      i.check.name = "point_number",
      i.check.current_value = "",
      i.check.value = "",
      i.check.issue_id = "duplicate_uuid",
      i.check.issue = "The uuid: {i.check.uuid} is duplicate in the data",
      i.check.other_text = "",
      i.check.checked_by = "",
      i.check.checked_date = as_date(today()),
      i.check.comment = "", 
      i.check.reviewed = "",
      i.check.adjust_log = "",
      i.check.uuid_cl = "",
      i.check.so_sm_choices = "")%>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# Outliers ----------------------------------------------------------------

check_outliers <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit) {
  input_tool_data %>% 
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>% 
    mutate(i.check.type = "remove_survey",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "",
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs engagement with enumerator"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl ="",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# average time of survey --------------------------------------------------

get_average_survey_time <- function(input_tool_data) {
  
  df_tool_data_with_time_interval <- input_tool_data %>% 
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval)
    )%>% 
    select(int.survey_time_interval)
    
  # lower and upper quantiles of survey duration
  lower_limit = quantile(df_tool_data_with_time_interval$int.survey_time_interval, 0.025)
  upper_limit = quantile(df_tool_data_with_time_interval$int.survey_time_interval, 0.97)
  
  df_tool_data_with_time_interval %>% 
    filter(int.survey_time_interval > lower_limit | int.survey_time_interval < upper_limit) %>% 
    summarise(average_time = round(mean(int.survey_time_interval, na.rm = TRUE), 0)) %>% 
    pull()
}



# function: extract others checks  ----------------------------------------

extract_other_data <- function(input_tool_data, input_survey, input_choices) {
  
  # add and rename some columns
  df_data <- input_tool_data %>% 
    rename(uuid = `_uuid`) %>% 
    mutate(start_date = as_date(start))
  
  # get questions with other
  others_colnames <-  df_data %>% 
    select(ends_with("_other"), -contains("/")) %>% 
    colnames()
  
  # data.frame for holding _other response data
  df_other_response_data <- data.frame()
  
  for (cln in others_colnames) {
    
    current_parent_qn = str_replace(string = cln, pattern = "_other$", replacement = "")
    
    df_filtered_data <- df_data %>% 
      select(-contains("/")) %>% 
      select(uuid, start_date, settlement, other_text = cln, current_value = current_parent_qn) %>% 
      filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
      mutate( other_name = cln, 
              int.my_current_val_extract = ifelse(str_detect(current_value, "\\bother\\b"), 
                                                  str_extract_all(string = current_value, pattern = "\\bother\\b|\\w+_other\\b"), 
                                                  current_value),
              value = "",
              parent_qn = current_parent_qn)
    df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
  }
  
  # arrange the data
  df_data_arranged <- df_other_response_data %>% 
    arrange(start_date, uuid)
  
  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : ")) %>% 
    arrange(list_name)
  
  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>% 
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
    rename(name = parent_qn)
  
  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>% 
    left_join(df_grouped_choices, by = "list_name") %>% 
    mutate(issue_id = "other_checks",
           issue = "",
           checked_by = "",
           checked_date = as_date(today()),
           comment = "",
           reviewed = "",
           adjust_log = ""
    ) %>% 
    filter(str_detect(string = current_value, pattern = "other\\b|\\w+_other\\b"))
  
  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_one|select one"))) %>% 
    mutate(type = "change_response")
  
  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_multiple|select multiple")))
  
  select_mu_add_option <- select_mu_data %>% 
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>% 
    mutate(type = "remove_option",
           value = as.character(int.my_current_val_extract))
  
  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>% 
    arrange(uuid, start_date, name)
  
  # merge other checks
  merged_other_checks <- bind_rows(output) %>% 
    mutate(so_sm_choices = choice_options) %>% 
    select(uuid,
           start_date,
           settlement,
           type,
           name,
           current_value,
           value,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           so_sm_choices)
}


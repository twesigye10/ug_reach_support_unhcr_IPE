library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
options("openxlsx.withFilter" = FALSE)

# tool
loc_tool <- "inputs/Individual_Profiling_Exercise_Tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") %>% 
  select(list_name, choice_name = name,   choice_label =label)

# analysis request
df_analysis_req <- readxl::read_excel("support_files/IPE V4 analysis request/IPE V4 Data Analysis Request.xlsx",
                                      sheet = "Recoded indicator list") %>% 
  janitor::clean_names()

# extract select types
df_tool_select_type <- df_survey %>% 
  select(type, qn_name = name, label) %>% 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# extract choice ids and labels
df_choices_support <- df_choices %>% 
  left_join(df_tool_select_type) %>% 
  unite("survey_choice_id", qn_name, choice_name, sep = "_", remove = FALSE) %>% 
  select(survey_choice_id, choice_label) 

# extract extra info from analysis request
df_extra_analysis_info <- df_analysis_req %>% 
  mutate(extra_info_code = ifelse(is.na(used_indicator_name), new_var_names_use_these_as_reference_for_the_analysis_script, used_indicator_name))

# analysis ----------------------------------------------------------------
# read analysis

df_lcsi_lab_support <- df_survey %>% 
  filter(str_detect(string = name, pattern = "^lcsi_"),
         !str_detect(string = type, pattern = "group|repeat|text|geopoint|^gps$|^note$")) %>% 
  select(name, label)

df_unformatted_analysis <- readxl::read_excel("outputs/analysis_ipe_hh_sampled_filtered.xlsx", sheet = "HH level analysis") %>% 
  mutate(int.analysis_var = ifelse(str_detect(string = analysis_var, pattern = "^i\\."), str_replace(string = analysis_var, pattern = "^i\\.", replacement = ""), analysis_var),
         analysis_choice_id = ifelse(int.analysis_var %in% c("unmet_needs_single_f_hoh"), paste0("unmet_needs_", analysis_var_value) ,paste0(int.analysis_var, "_", analysis_var_value)),
         analysis_var_value_label = ifelse(analysis_choice_id %in% df_choices_support$survey_choice_id, recode(analysis_choice_id, !!!setNames(df_choices_support$choice_label, df_choices_support$survey_choice_id)), analysis_var_value),
         Indicator = ifelse(analysis_var %in% df_extra_analysis_info$extra_info_code, recode(analysis_var, !!!setNames(df_extra_analysis_info$indicator, df_extra_analysis_info$extra_info_code)), analysis_var),
         Indicator = ifelse(!analysis_var %in% df_extra_analysis_info$extra_info_code & analysis_var %in% df_tool_select_type$qn_name, 
                            recode(analysis_var, !!!setNames(df_tool_select_type$label, df_tool_select_type$qn_name)), Indicator),
         Sector = ifelse(analysis_var %in% df_extra_analysis_info$extra_info_code, recode(analysis_var, !!!setNames(df_extra_analysis_info$sector, df_extra_analysis_info$extra_info_code)), NA_character_),
  )

# create wide analysis
df_analysis_wide <- df_unformatted_analysis %>% 
  select(-any_of(c("group_var", "stat_low", "stat_upp", 
            "n_total", "n_w", "n_w_total", "analysis_key", "population"))) %>% 
  # n_total, n_w, n_w_total, analysis_key)) %>% 
  mutate(group_var_value = ifelse(is.na(group_var_value), "total", group_var_value)) %>% 
  pivot_wider(names_from = c(group_var_value), values_from = c(stat, n)) %>% 
  # pivot_wider(names_from = c(group_var_value, population), values_from = c(stat, n)) %>% 
  mutate(row_id = row_number())


# openxlsx::write.xlsx(df_analysis_wide, "outputs/prepare_4_column_ordering_ipe_sampled.xlsx", overwrite = T)

df_cols_for_ordering <- readxl::read_excel("outputs/column_ordering_ipe_sampled.xlsx", sheet = "combined")

reordered_columns <- df_cols_for_ordering %>%
  pivot_longer(cols = c(result_col, n_unweighted), names_to = "entries", values_to = "columns") %>%
  pull(columns)

# reorder
extra_cols_for_analysis_tables <- c("Indicator")
df_analysis_wide_reodered <- df_analysis_wide %>%
  # relocate(any_of(extra_cols_for_analysis_tables), .before = "analysis_var") %>% 
  relocate("analysis_var_value_label", .after = "analysis_var_value") %>% 
  relocate(any_of(reordered_columns), .after = "analysis_var_value_label") %>% 
  relocate(analysis_type, .after = "analysis_var_value_label")

cols_for_num_pct_formatting <- df_analysis_wide_reodered %>% 
  # select(stat_total:row_id, - c("Indicator", "Sector")) %>% 
  select(stat_total:row_id, - c("Indicator", "Sector")) %>% 
  select(!matches("^n_"), -row_id) %>% 
  colnames()

# extract header data

df_to_extract_header = df_analysis_wide_reodered %>% 
  select(-any_of(c("analysis_var", "analysis_var_value", "analysis_var_value_label",
                   "int.analysis_var", "analysis_choice_id", "Indicator", "row_id"))) %>% 
  colnames()

df_extracted_header_data <- tibble("old_cols" = df_to_extract_header) %>% 
  mutate("new_cols" = paste0("x", row_number())) %>% 
  mutate(old_cols = str_replace(string = old_cols, pattern = "Results\\(mean\\/percentage\\)_|_host_community$|_refugee$", replacement = "")) %>%
  mutate(old_cols = str_replace(string = old_cols, pattern = "_host_community_NA$|_refugee_NA$", replacement = "")) %>%
  mutate(old_cols = str_replace(string = old_cols, pattern = "^n_.+", replacement = "n")) %>% 
  mutate(old_cols = str_replace(string = old_cols, pattern = "%/%", replacement = "")) %>% 
  pivot_wider(names_from = new_cols, values_from = old_cols)

df_extracted_header <- bind_rows(df_extracted_header_data) %>% 
  mutate(x1 = "Analysis Type")

# create workbook ---------------------------------------------------------

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T, 
                   border = "TopBottomLeftRight", borderStyle = "medium", borderColour = "#000000")
hs2 <- createStyle(fgFill = "grey", halign = "LEFT", textDecoration = "Bold", fontColour = "white", wrapText = F)
hs2_no_bold <- createStyle(fgFill = "grey", halign = "LEFT", textDecoration = "", fontColour = "white", wrapText = F)
hs2_relevant <- createStyle(fgFill = "grey", halign = "LEFT", textDecoration = "", fontColour = "#808080", wrapText = F)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", fontColour = "white", textDecoration = "Bold", 
                   border = "TopBottomLeftRight", borderStyle = "medium", borderColour = "#000000")

modifyBaseFont(wb = wb, fontSize = 12, fontName = "Arial Narrow")

# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")
# percent
pct = createStyle(numFmt="0.0%") # not working

addWorksheet(wb, sheetName="Analysis - IPE Sampled")

# header showing results headings
writeData(wb, sheet = "Analysis - IPE Sampled", df_extracted_header %>% head(1), startCol = 2, 
          startRow = 1, headerStyle = hs2, colNames = FALSE, 
          borders = "all", borderColour = "#000000", borderStyle = "thin")

setColWidths(wb = wb, sheet = "Analysis - IPE Sampled", cols = 1, widths = 70)
setColWidths(wb = wb, sheet = "Analysis - IPE Sampled", cols = 2, widths = 16)
setColWidths(wb = wb, sheet = "Analysis - IPE Sampled", cols = 3:4, widths = 10)
setColWidths(wb = wb, sheet = "Analysis - IPE Sampled", cols = 5:80, widths = 10)
setColWidths(wb = wb, sheet = "Analysis - IPE Sampled", cols = 81, widths = 20)

# split variables to be written in different tables with in a sheet
sheet_variables_data <- split(df_analysis_wide_reodered, factor(df_analysis_wide_reodered$analysis_var, levels = unique(df_analysis_wide_reodered$analysis_var)))

previous_row_end <- 1

for (i in 1:length(sheet_variables_data)) {
  
  current_variable_data <- sheet_variables_data[[i]]
  
  get_question <- current_variable_data %>% select(analysis_var) %>% unique() %>% pull()
  get_question_label <- current_variable_data %>% select(Indicator) %>% unique() %>% pull()
  get_qn_type <- current_variable_data %>% select(analysis_type) %>% unique() %>% pull()
  
  if(get_qn_type %in% c("prop_select_one", "prop_select_multiple")){
    for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "percentage"}
  }else{
    for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "numeric"}
  }
  
  # this controls rows between questions
  current_row_start <- previous_row_end + 2
  
  # print(paste0("current start row: ", current_row_start, ", variable: ", get_question))
  
  # add header for variable
  writeData(wb, sheet = "Analysis - IPE Sampled", get_question_label, startCol = 1, startRow = previous_row_end + 1)
  writeData(wb, sheet = "Analysis - IPE Sampled", get_qn_type, startCol = 2, startRow = previous_row_end + 1)
  addStyle(wb, sheet = "Analysis - IPE Sampled", hs2, rows = previous_row_end + 1, cols = 1:2, gridExpand = TRUE)
  
  # current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)
  current_data_length <- nrow(current_variable_data)
  
  print(paste0("start row: ", current_row_start, ", previous end: ", previous_row_end, ", data length: ", current_data_length, ", variable: ", get_question))
  
  writeData(wb = wb, 
            sheet = "Analysis - IPE Sampled", 
            x = current_variable_data %>% 
              select(-c(analysis_var, int.analysis_var, analysis_var_value,
                        analysis_choice_id, Indicator,
                        row_id)
              ) %>% 
              mutate(analysis_type = NA_character_) %>% 
              arrange(desc(stat_total)), 
            startRow = current_row_start, 
            startCol = 1, 
            colNames = FALSE)
  
  previous_row_end <- current_row_start + current_data_length
}

# freeze pane
freezePane(wb, "Analysis - IPE Sampled", firstActiveRow = 2, firstActiveCol = 3)

# openXL(wb)

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_hh_sampled_filtered_IPE_formatted_analysis.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_hh_sampled_filtered_IPE_formatted_analysis.xlsx"))

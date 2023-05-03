library(tidyverse)
library(lubridate)
library(glue)


# verification ------------------------------------------------------------

ind_data_loc <- "inputs/REACH DataPWD/proGresDataIndividualToShare2.csv"
df_individual_data_prog <- readr::read_csv2(ind_data_loc)
colnames(df_individual_data_prog)

df_data_delim <- readr::read_delim(file = ind_data_loc, delim = ";")
colnames(df_data_delim)


ipe_data_adjumani <- "inputs/REACH DataPWD/Adjumani IPE 06042023.csv"
df_ipe_data_adjumani <- readr::read_delim(file = ipe_data_adjumani, delim = ";")
colnames(df_ipe_data_adjumani)

# sampled data ------------------------------------------------------------

sampled_data_loc <- "inputs/Reach Household visit/IPE_questionnaire_for_sampled_households_sqlite.xlsx"

# data_nms_sample <- names(readxl::read_excel(path = sampled_data_loc, n_max = 2000))
# c_types_sample <- ifelse(str_detect(string = data_nms_sample, pattern = "^col1$|^col2$|^col3$"), "date", "guess")

df_hh_sampled_data <- readxl::read_excel(sampled_data_loc)
# colnames(df_hh_sampled_data)


# check that the key works for both datasets ------------------------------

df_hh_sampled_data_id <- df_hh_sampled_data |> select(GroupAnonymized)

df_data_delim_id <- df_data_delim |> select(AnonymizedGrp) |> pull()

df_hh_sampled_data |> filter(GroupAnonymized %in% df_data_delim$AnonymizedGrp) |> nrow()



df_hh_sampled_data_id |> filter(GroupAnonymized %in% df_data_delim_id)


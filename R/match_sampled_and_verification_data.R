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

data_path_verification <- "outputs/combined_ipe_verif_data_with_batch_name.csv"

df_ipe_data_complete_verification <- readr::read_csv(file =  data_path_verification)

  
# sampled data ------------------------------------------------------------

sampled_data_loc <- "inputs/Reach Household visit/IPE_questionnaire_for_sampled_households_sqlite.xlsx"

# data_nms_sample <- names(readxl::read_excel(path = sampled_data_loc, n_max = 2000))
# c_types_sample <- ifelse(str_detect(string = data_nms_sample, pattern = "^col1$|^col2$|^col3$"), "date", "guess")

df_hh_sampled_data <- readxl::read_excel(sampled_data_loc)
# colnames(df_hh_sampled_data)


# check that the key works for both datasets ------------------------------

# df_hh_sampled_data_id <- df_hh_sampled_data |> select(GroupAnonymized)
# 
# df_data_delim_id <- df_data_delim |> select(AnonymizedGrp) |> pull()
# 
# df_hh_sampled_data |> filter(GroupAnonymized %in% df_data_delim$AnonymizedGrp) |> nrow()
# 
# df_hh_sampled_data_id |> filter(GroupAnonymized %in% df_data_delim_id)

# sampled data matching

df_id_mappings <- readr::read_csv("inputs/REACH DataPWD/RegistrationGroupDataMappingKeys.csv")

df_sampled_matching <- df_hh_sampled_data |> 
  filter(GroupAnonymized %in% df_id_mappings$GroupAnonymized)

# nrow(df_hh_sampled_data) ##: 23835

nrow(df_sampled_matching) ## : 16115

# deficit: 7720
# deficit
df_sampled_not_matching <- df_hh_sampled_data |> 
  filter(!GroupAnonymized %in% df_id_mappings$GroupAnonymized)
nrow(df_sampled_not_matching)
df_sampled_not_matching |> select(GroupAnonymized) |> 
  write_csv("outputs/un_matching_GroupAnonymized_sampled_data.csv")


# verification data matching
df_verification_matching <- df_ipe_data_complete_verification |> 
  filter(AnonymizedGrp %in% df_id_mappings$AnonymizedGrp)

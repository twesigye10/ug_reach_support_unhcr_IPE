library(tidyverse)
library(lubridate)
library(glue)


# verification ------------------------------------------------------------

ind_data_loc <- "inputs/REACH DataPWD/proGresDataIndividualToShare2.csv"
df_individual_data_prog <- readr::read_csv2(ind_data_loc)
colnames(df_individual_data_prog)

df_data_delim <- readr::read_delim(file = ind_data_loc, delim = ";")
colnames(df_data_delim)
# verification adjumani
ipe_data_adjumani <- "inputs/REACH DataPWD/Adjumani IPE 06042023.csv"
df_ipe_data_adjumani <- readr::read_delim(file = ipe_data_adjumani, delim = ";")
colnames(df_ipe_data_adjumani)

data_path_verification <- "outputs/combined_ipe_verif_data_with_batch_name.csv"

df_ipe_data_complete_verification <- readr::read_csv(file =  data_path_verification)
colnames(df_ipe_data_complete_verification)
  
# sampled data ------------------------------------------------------------

sampled_data_loc <- "inputs/Reach Household visit/IPE_questionnaire_for_sampled_households_sqlite_2023_05_12.xlsx"

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

df_id_mappings <- readr::read_csv("inputs/REACH DataPWD/Registration_Mapping_allTablesFinal_shared_2023_05_12.csv")
colnames(df_id_mappings)
# "GroupAnonymizedIPEHHVisit" "IndividualproGresData"     "AnonymizeGroupProGresData"
# "GroupAnonymizedIPEIndiv"   "IndivAnonymizedIPEIndiv"
df_sampled_matching <- df_hh_sampled_data |> 
  filter(GroupAnonymized %in% df_id_mappings$GroupAnonymizedIPEHHVisit)

# nrow(df_hh_sampled_data) ##: 23835

nrow(df_sampled_matching) ## : 16115

# deficit: 7720
# deficit
df_sampled_not_matching <- df_hh_sampled_data |> 
  filter(!GroupAnonymized %in% df_id_mappings$GroupAnonymizedIPEHHVisit) # GroupAnonymizedIPEHHVisit
nrow(df_sampled_not_matching)
df_sampled_not_matching |> select(GroupAnonymized, settlement) |> 
  write_csv("outputs/un_matching_GroupAnonymized_sampled_data.csv")


# verification data matching
df_verification_matching <- df_ipe_data_complete_verification |> 
  filter(progres_individualid %in% df_id_mappings$IndividualproGresData) # IndividualproGresData
# not matching verification data
df_verification_not_matching <- df_ipe_data_complete_verification |> 
  filter(!progres_individualid %in% df_id_mappings$IndividualproGresData) # IndividualproGresData


# check dataset shared on 27/09/2023 --------------------------------------

# shared on 29/08/2023
verif_data_loc1 <- "inputs/IPE_Updated/Reach data IPE 2908.csv"
df_verification_data1 <- readr::read_csv(verif_data_loc1, na = c("NULL", "", "NA"))

sampled_data_loc1 <- "inputs/IPE_Updated/Reach data Household Visit 2908.csv"
df_sampled_data1 <- readr::read_csv(sampled_data_loc1, na = c("NULL", "", "NA"))

# shared on 27/09/2023
verif_data_loc2 <- "inputs/IPE_Updated/updated 20230927/Reach data IPE data 27 Sep 2023 - Missing Cols.xlsx"
df_verification_data2 <- readxl::read_excel(verif_data_loc2, na = c("NULL", "", "NA"))
nrow(df_verification_data2) # 89853
# shared on 26/09/2023 [has specific columns that had not been asked]
verif_data_loc3 <- "inputs/IPE_Updated/Missing from the Verification dataset.xlsx"
df_verification_data3 <- readxl::read_excel(verif_data_loc3, na = c("NULL", "", "NA"))
nrow(df_verification_data3) # 89659

# check if the ids of the two extra datasets match
df_verification_data2 %>% 
  filter(AnonymizedInd %in% df_verification_data3$AnonymizedInd) %>% 
  nrow() # 89659

sampled_data_loc2 <- "inputs/IPE_Updated/updated 20230927/Reach data Household Visit 2908_20230927.xlsx"
df_sampled_data2 <- readxl::read_excel(sampled_data_loc2, na = c("NULL", "", "NA"))

# compare
df_verification_data1 %>% 
  filter(AnonymizedInd %in% df_verification_data2$AnonymizedInd)

df_verification_data1 %>% 
  filter(AnonymizedGrp %in% df_verification_data2$AnonymizedGrp)


df_sampled_data2 %>% 
  filter(anonymizedgroup %in% df_verification_data1$AnonymizedGrp) # 21,281

nrow(df_sampled_data2) # 22628

df_sampled_data2 %>% 
  filter(anonymizedgroup %in% df_verification_data2$AnonymizedGrp) # 21,320

# clean HH data
clean_hh_data_loc1 <- "inputs/clean_data_ipe_hh_sampled.xlsx"
df_clean_hh_data1 <- readxl::read_excel(clean_hh_data_loc1)

df_clean_hh_data1 %>% 
  filter(uuid %in% df_sampled_data2$`_uuid`) # 19,390

nrow(df_clean_hh_data1) # 20191

# there were duplicate uuid and index column values with 

sampled_data_loc3 <- "inputs/IPE_Updated/updated 20230927/HHs data.xlsx"
df_sampled_data3 <- readxl::read_excel(sampled_data_loc3)

df_clean_hh_data1 %>% 
  filter(uuid %in% df_sampled_data3$`_uuid`) %>% 
  nrow() # 20191

df_sampled_data3 %>% 
  filter(anonymizedgroup %in% df_verification_data1$AnonymizedGrp) %>% 
  nrow() # 21298

df_sampled_data3 %>% 
  filter(!anonymizedgroup %in% df_verification_data1$AnonymizedGrp) %>% 
  nrow() # 2923

df_sampled_data3 %>% 
  filter(!anonymizedgroup %in% df_verification_data1$AnonymizedGrp) %>% 
  select(`_uuid`, anonymizedgroup, col3, settlement, zone)  %>% 
  write_csv("outputs/sample_data_not_in_verification.csv")

df_sampled_data3 %>% 
  filter(anonymizedgroup %in% df_verification_data2$AnonymizedGrp) %>% 
  nrow() # 21337



df_clean_hh_data1 %>% left_join(df_sampled_data3 %>% select(uuid = `_uuid`, anonymizedgroup)) %>% 
  filter(anonymizedgroup %in% df_verification_data2$AnonymizedGrp) %>% 
  nrow() # 19003

df_clean_hh_data1 %>% left_join(df_sampled_data3 %>% select(uuid = `_uuid`, anonymizedgroup)) %>% 
  filter(anonymizedgroup %in% df_verification_data1$AnonymizedGrp) %>% 
  nrow() # 19003

nrow(df_sampled_data3) # 24221

data_for_update <- df_sampled_data3

# combine verification data -----------------------------------------------

# combined_ipe_verif_data_20230927.csv

df_updated_cols_verif <- df_verification_data2 %>% 
  select(-c("businessunitname", "AnonymizedGrp",    "progres_size",
            "progres_relationshiptofpname",  "progres_coalocationlevel1name",
            "progres_coalocationlevel2name", "progres_countryoforiginidname", "progres_sexname",
            "37", "38", "41", "60")) %>% 
  group_by(AnonymizedInd) %>% 
  filter(row_number() == 1) %>% 
  ungroup()  # 89658 // 3 duplicated entries
  
df_updated_cols_verif_3 <- df_verification_data3 %>% 
  select(AnonymizedInd, "11", "12") %>% 
  group_by(AnonymizedInd) %>% 
  filter(row_number() == 1) %>% 
  ungroup()  # 89656 // 3 duplicated entries
  

df_combined_ipe <- df_verification_data1 %>% 
  group_by(AnonymizedInd) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  left_join(df_updated_cols_verif, by = "AnonymizedInd")  %>% 
  left_join(df_updated_cols_verif_3, by = "AnonymizedInd") # 89656

nrow(df_verification_data1) # 89658 // 2 duplicated entries
nrow(df_combined_ipe) # 89656

df_verification_data1 %>% 
  group_by(AnonymizedInd) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  nrow()

nrow(df_verification_data2) # 89853 // 3 duplicated entries

df_updated_ver <- df_verification_data2 %>% 
  group_by(AnonymizedInd) %>% 
  filter(n() > 1) %>% 
  ungroup() #%>% 
  # nrow() # 89850

write_csv(df_combined_ipe, paste0("outputs/", butteR::date_file_prefix(), "_combined_ipe_verif_data.csv"), na="")
write_csv(df_combined_ipe, paste0("outputs/combined_ipe_verif_data.csv"), na="")
write_csv(df_combined_ipe, paste0("inputs/combined_ipe_verif_data.csv"), na="")

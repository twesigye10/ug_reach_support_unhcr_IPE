library(tidyverse)
library(lubridate)
library(glue)


# combine verification data ----------------------------------------------------

verif_data_loc <- "inputs/REACH DataPWD/"

df_ipe_verif_data_list <- list.files(path = verif_data_loc, 
                               pattern = " IPE ", full.names = T) |> 
  set_names()

df_combined_verif_data <- purrr::map_dfr(.x = df_ipe_verif_data_list,
                                       .f = ~ readr::read_delim(file = ., delim = ";"),
                                         .id = "batch_name") |> 
  mutate(batch_name = str_replace(string = batch_name, pattern = "inputs/REACH DataPWD/", replacement = ""),
         batch_name = str_replace(string = batch_name, pattern = "\\.csv$", replacement = ""))

rio::export(df_combined_verif_data, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_ipe_verif_data_with_batch_name.xlsx"))


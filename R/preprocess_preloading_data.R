library(tidyverse)

df_preloading_data <- read_csv("inputs/sample_data_for_hh_visit.csv")

# "Registration_Group", "IndividualID", "Settlement", "Zone", "Village", "Relationship", "Sex", "Age" 

df_hh_size <- df_preloading_data %>% 
  group_by(Registration_Group) %>%
  summarise(hh_size = n())


df_data_with_hh_size <- df_preloading_data %>% 
  distinct(Registration_Group, Settlement, Zone, Village) %>% 
  left_join(df_hh_size, by = "Registration_Group")

write_csv(x = df_data_with_hh_size, file = "outputs/hh_level_data")
library(DBI)

# read data 
# df_ipe_data_db <- dbConnect(RSQLite::SQLite(), "inputs/Reach Household visit/dbHHVisit.db") # original data
df_ipe_data_db <- dbConnect(RSQLite::SQLite(), "inputs/Reach Household visit/dbHHVisit_shared_2023_05_12.db")

# list the tables in the database
dbListTables(df_ipe_data_db)

# list the tables in the database
dbListFields(df_ipe_data_db, "t_profiling10percentHH")
dbListFields(df_ipe_data_db, "t_profiling10percentHHCSV")

# get the data 
# df_ipe_data <- dbGetQuery(df_ipe_data_db, 'SELECT * FROM t_profiling10percentHH') # original data
df_ipe_data <- dbGetQuery(df_ipe_data_db, 'SELECT * FROM t_profiling10percentHHCSV')

nrow(df_ipe_data)

# janitor::row_to_names(df, 1, remove_rows_above = FALSE) 
df_new_data <- janitor::row_to_names(df_ipe_data, 1, remove_rows_above = TRUE) 

openxlsx::write.xlsx(x = df_new_data, file = "inputs/Reach Household visit/IPE_questionnaire_for_sampled_households_sqlite_2023_05_12.xlsx")


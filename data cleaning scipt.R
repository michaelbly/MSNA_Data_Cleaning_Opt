library(dplyr)
library(lubridate)
library(readxl)
library(kableExtra)
library(knitr)
library(readr)
library(openxlsx)
library(sf)
library(raster)
library(Hmisc)
source("functions/audit_function_full.R")
source("functions/function_handler.R")

WGS84 <- crs("+init=epsg:4326")
UTM38N <- crs("+init=epsg:32638")

assessment_start_date <- as.Date("2021-05-17")

# set min time and max # interviews per day

time_limit <- 15
flag_time_limit <- 30
max_interv <- 10

# read data from excel file
##df <- read_excel("input/raw_data/Final__V10__REACH_oPt_Kobo_MSNA_May_2021_V8_27052021_-_all_versions_-_False_-_2021-06-07-08-58-28.xlsx", sheet = "MSNA_I_2021")
  
##df <- read_csv("input/raw_data/Final__V10__REACH_oPt_Kobo_MSNA_May_2021_V8_27052021_-_all_versions_-_False_-_2021-06-07-08-58-28.csv")
df <- read_excel("input/raw_data/FINAL_Pilot_V12_REACH_oPt_Kobo_MSNA_June_2021_13062021_-_all_versions_-_False_-_2021-06-16-07-13-11.xlsx")

###########################################################################################################
# time check from audit files
# today
df$today <- as.Date(df$start, "%Y-%m-%d")

df <- time_check_audit(audit_dir_path = "audit/", df,  "_uuid", time_limit = time_limit)

df <- time_check(df, time_limit)
df <- df %>% 
  mutate(time_validity = case_when(interview_duration < flag_time_limit & 
                                     interview_duration > time_limit ~ "Flagged", TRUE ~ time_validity))

###########################################################################################################



# when survey does not continue to hh member calculation, these surveys are not valid

df <- df %>% 
  mutate(not_eligible = case_when(is.na(hh_size) ~ "yes",
                                  TRUE ~ "no"))
df$not_eligible
# import loop data
indiv_df <- read_excel("input/raw_data/Final__V10__REACH_oPt_Kobo_MSNA_May_2021_V8_27052021_-_all_versions_-_False_-_2021-06-07-08-58-28.xlsx", sheet = "member")

indiv_df <- indiv_df %>%
            mutate('_uuid' = '_submission__uuid')


##########################################################################################
# Deleted interviews column
df <- df %>% 
  mutate(
    deleted = case_when(
      time_validity == "Deleted" | consent == "no" | not_eligible == "yes" ~ "yes",
      TRUE ~ "no"))
df$deleted



########################################################################################
###########################################################################################################
# calculate new variables


# number of NAs check
df$NAs <- apply(df,1,FUN=function(x){length(which(is.na(x)))})

##########################################################################################################
### EXPORT FOR DATA CHECKING #############

##df<- df %>% 
##dplyr::select(-(snowballing_willing:`_gpslocation_precision`))

##### Write to csv for data checking ###################
write.csv(df, sprintf("output/data_checking/mcna_all_data_%s.csv",today()), row.names = F)





###########################################################################################################
###########################################################################################################
###########################################################################################################
# DO CLEANING
# read cleanimg conditions csv list
conditionDf_1 <- read.csv("input/conditions/conditions_log1.csv", as.is = TRUE)

# return logs

logs <- read_conditions_from_excel_limited_row(df, conditionDf_1);

# create new columns "log_number"
logs$log_number = seq.int(nrow(logs))
# order data frame by log_number
ordered_df <- logs[order(logs$log_number),]
readr::write_excel_csv(ordered_df, sprintf("output/cleaning_log/cleaning_log_%s.csv",today()))

# export data with check columns
logs_as_columns <- read_conditions_from_excel_column(df, conditionDf_1);
write.csv(logs_as_columns, sprintf("output/cleaning_log/data_w_checks/data_checks_%s.csv",today()), row.names = FALSE)

#################################################################################################################
# read log csv file after AO has indicated decision on flagged data
log_df <- read.csv(sprintf("output/cleaning_log/cleaning_log_%s.csv",today()), as.is = TRUE)
replaced_df <- read_logs(df, log_df, conditionDf_1) 

# take uuids of deleted surveys and remove from cleaned dataset
deleted_surveys <- replaced_df %>% 
  filter(deleted == "yes")

deleted_uuids <- deleted_surveys %>% 
  dplyr::select(`_uuid`, deleted = time_validity)

# ##join with individual data set to remove deleted uuids from loop
indiv_df <- left_join(indiv_df, deleted_uuids, by = "_uuid") 

###take uuids of deleted loop surveys and remove from cleaned dataset
deleted_loop <- indiv_df %>% 
filter(!is.na(deleted))

indiv_cleaned <- indiv_df %>%  filter(is.na(deleted))

# remove deleted surveys from cleaned dataset
replaced_df %<>% filter(deleted == "no")

### Generating the cluster id
#replaced_df <- replaced_df %>% 
#mutate(
# oslo_area =replace(oslo_area,is.na(oslo_area), "gaza"),
# cluster_id =paste(enumeration_area,locality_code,oslo_area, sep = "_")) %>%
# dplyr::select(cluster_id,enumeration_area,locality_code, oslo_area )

# export clean data
write.csv(replaced_df, sprintf("output/cleaned_data/mcna_data_clean_parent_%s.csv",today()), row.names = FALSE)
write.csv(indiv_df, sprintf("output/cleaned_data/mcna_data_clean_loop_%s.csv",today()), row.names = FALSE)

# export to one spreadsheet
mcna_datasets <- replaced_df
write.xlsx(mcna_datasets, (file = sprintf("output/cleaned_data/mcna_data_clean_%s.xlsx", today())))

# export deletion log
mcna_deleted <- deleted_surveys
write.xlsx(mcna_deleted, (file = sprintf("output/deletion_log/mcna_data_deleted_%s.xlsx", today())))


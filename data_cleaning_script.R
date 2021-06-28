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

assessment_start_date <- as.Date("2021-06-16")

# set min time and max # interviews per day

time_limit <- 10
flag_time_limit <- 20
max_interv <- 10

df <- read_excel("input/raw_data/FINAL_Pilot_V12_REACH_oPt_Kobo_MSNA_June_2021_13062021_2021-06-20-05-33-16.xlsx", sheet = "MSNA_I_2021")
      

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
df$time_validity



# when survey does not continue to hh member calculation, these surveys are not valid

df <- df %>% 
  mutate(not_eligible = case_when(is.na(hh_size) ~ "yes",TRUE ~ "no"),
         uuid = `_uuid`,
         X_uuid = uuid)


df$not_eligible


# import loop data
indiv_df <- read_excel("input/raw_data/FINAL_Pilot_V12_REACH_oPt_Kobo_MSNA_June_2021_13062021_2021-06-20-05-33-16.xlsx", sheet = "member") %>% 
            mutate(uuid = `_submission__uuid`,
                   X_submission__uuid = uuid)

############ add full strata
######NOTE ::: CHANGE THE OSLO AREA TO REFLECT THE TOOL, A, B, C
df <- df %>%
  mutate(across(everything(), as.character)) %>%
  mutate(strata =
           case_when(location == "west_bank" & (oslo_area == "area_a" | oslo_area == "area_b") ~ "Area_AB",
                     location == "west_bank" & oslo_area == "area_c" ~ paste(hh_location_wb, oslo_area, sep = "_"),
                     location == "gaza" ~ hh_location_gaza,
                     location == "ej"~ "East_Jerusalem",
                     TRUE  ~  'H2'))


#### df after the time check
#df_raw <- df
#write.xlsx(df_raw, (file = sprintf("output/df_raw_%s.xlsx", today())))

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

###############################################################################
#DELETE NOTE COLUMNS
df <- df %>% 
  dplyr::select(-contains("_note"))
#############################################################################

##########################################################################################################
### EXPORT FOR DATA CHECKING #############



##### Write to csv for data checking ###################
write.csv(df, sprintf("output/data_checking/mcna_all_data_%s.csv",today()), row.names = F)

###########################################################################################################
###########################################################################################################
###########################################################################################################
# DO CLEANING
# read cleanimg conditions csv list
conditionDf <- read.csv("input/conditions/conditions_log.csv", as.is = TRUE)

# return logs

logs <- read_conditions_from_excel_limited_row(df, conditionDf);

# create new columns "log_number"
logs$log_number = seq.int(nrow(logs))
# order data frame by log_number
ordered_df <- logs[order(logs$log_number),]
readr::write_excel_csv(ordered_df, sprintf("output/cleaning_log/cleaning_log_%s.csv",today()))

# export data with check columns
logs_as_columns <- read_conditions_from_excel_column(df, conditionDf);
write.csv(logs_as_columns, sprintf("output/cleaning_log/data_w_checks/data_checks_%s.csv",today()), row.names = FALSE)

#################################################################################################################
# read log csv file after AO has indicated decision on flagged data
log_df <- read.csv(sprintf("output/cleaning_log/cleaning_log_%s.csv",today()), as.is = TRUE)
replaced_df <- read_logs(df, log_df, conditionDf) 


replaced_df$deleted

# take uuids of deleted surveys and remove from cleaned dataset
deleted_surveys <- replaced_df %>% 
                   filter(deleted == "yes")



deleted_uuids <- deleted_surveys %>% 
  dplyr::select('_uuid', deleted = time_validity)

# ##join with individual data set to remove deleted uuids from loop
indiv_df <- left_join(indiv_df, deleted_uuids, by = c("_submission__uuid"="_uuid")) 

###take uuids of deleted loop surveys and remove from cleaned dataset
deleted_loop <- indiv_df %>% 
filter(!is.na(deleted))

indiv_cleaned <- indiv_df %>%  filter(is.na(deleted))

# remove deleted surveys from cleaned dataset
replaced_df %<>% filter(deleted == "no")


# export clean data
write.csv(replaced_df, sprintf("output/cleaned_data/mcna_data_clean_parent_%s.csv",today()), row.names = FALSE)
write.csv(indiv_df, sprintf("output/cleaned_data/mcna_data_clean_loop_%s.csv",today()), row.names = FALSE)

# export to one spreadsheet
mcna_datasets <- list("MCNA_2021" = replaced_df, "member" = indiv_cleaned)
write.xlsx(mcna_datasets, (file = sprintf("output/cleaned_data/mcna_data_clean_%s.xlsx", today())))

# export deletion log
mcna_deleted <- list("MCNA_2021" = deleted_surveys, "member" = deleted_loop)
write.xlsx(mcna_deleted, (file = sprintf("output/deletion_log/mcna_data_deleted_%s.xlsx", today())))



# export to one spreadsheet
#mcna_datasets <-
 # list(
  #  "MCNA_2021" = replaced_df,
   # "member" = indiv_cleaned,
   # "cleaning_log_hh" = log_df,
    #"deletion_log" = deleted_redacted
 # )

#write.xlsx(mcna_datasets, (file = sprintf("output/cleaned_data/mcna_data_clean_all_%s.xlsx", today())))


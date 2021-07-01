setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
library(leaflet)
library(htmlwidgets)
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
# [GPS] STRATA CHECK
###########################################################################################################
# load shape files
gov_WB <- st_read("input/shapes/Governorates_WB/Governorates_WB.shp") %>% st_transform(crs = 4326)
oslo <- st_read("input/shapes/Oslo_EJ_NML/Oslo_EJ_NML.shp") %>% st_transform(crs = 4326)
# load settlements GDB file
dir.settlements <- "input/shapes/IsraeliSettlements.gdb/"
layers <- st_layers(dir.settlements)
settlements_outer <- st_transform(st_read(dir.settlements, layer=layers$name[1]), crs = 4326)
settlements_builtup <- st_transform(st_read(dir.settlements, layer=layers$name[2]), crs = 4326)
# merge outer and builup layer
settlements <- rbind(dplyr::select(settlements_builtup, Name_Eng) %>% rename(Name=Name_Eng),
                     dplyr::select(settlements_outer, Name))
settlements <- st_cast(st_union(settlements), "POLYGON")

# keep only samples with GPS coordinates and convert dataframe to sf
df_wb_sf <- df %>% 
  filter(location=="west_bank" & !is.na(gpslocation)) %>% 
  st_as_sf(coords=c("_gpslocation_longitude", "_gpslocation_latitude"), crs = 4326)

# add governorate and oslo information based on GPS coordinates
df_wb_sf <- st_join(df_wb_sf, dplyr::select(gov_WB, GOVERNORAT))
df_wb_sf <- st_join(df_wb_sf, dplyr::select(oslo, CLASS))
# reformat governorate and oslo area
df_wb_sf <- df_wb_sf %>% 
  mutate(gps.governorate=tolower(GOVERNORAT),
         gps.oslo.area=case_when(
           CLASS=="AREA (A)" ~ "A",
           CLASS=="AREA (B)" ~ "B",
           CLASS=="Inferred AREA (C)" ~ "C",
           CLASS=="Special case" ~ "H2",
           TRUE ~ CLASS))

# [CHECK] generate map with governorate issues
df_wb_sf_issues <- filter(df_wb_sf, hh_location_wb!=gps.governorate)
View(dplyr::select(df_wb_sf_issues, hh_location_wb, oslo_area, gps.governorate, gps.oslo.area))
map <- leaflet() %>% 
  addPolygons(data=gov_WB, color = "#0080FF", weight = 1, fillOpacity=0.2, opacity = 0.8, label = gov_WB$GOVERNORAT) %>%
  addCircles(data = df_wb_sf_issues, stroke = F, fill = T, fillOpacity = 1, radius = 50, fillColor="#FF0000",
             label=paste0(df_wb_sf_issues$`_index`, ": ", df_wb_sf_issues$hh_location_wb, "-", df_wb_sf_issues$oslo_area)) %>%
  addTiles()
saveWidget(map, file="samples_gov.html")

# [CHECK] generate map with oslo.area issues
df_wb_sf_issues <- filter(df_wb_sf, oslo_area!=gps.oslo.area)
View(dplyr::select(df_wb_sf_issues, hh_location_wb, oslo_area, gps.governorate, gps.oslo.area))
map <- leaflet() %>% 
  addPolygons(data=oslo, 
              color = c("#0080FF", "#EE00FF", "#33FF00", "#FF9900", "#00ffff", "#ff0000", "#0000ff"), 
              weight = 1, fillOpacity=0.2, opacity = 0.8, label = oslo$CLASS) %>%
  addCircles(data = df_wb_sf_issues, stroke = F, fill = T, fillOpacity = 1, radius = 50, fillColor="#FF0000",
             label=paste0(df_wb_sf_issues$`_index`, ": ", df_wb_sf_issues$hh_location_wb, "-", df_wb_sf_issues$oslo_area)) %>%
  addTiles()
saveWidget(map, file="samples_oslo.html")

# generate cleaning log
issue1 <- "Sample GPS location is not within the specified governorate and Oslo area."
issue2 <- "Sample GPS location is not within the specified governorate."
issue3 <- "Sample GPS location is not within the specified Oslo area."
df_wb_sf_issues <- df_wb_sf %>% 
  dplyr::filter(hh_location_wb!=gps.governorate | oslo_area!=gps.oslo.area) %>% 
  dplyr::select("_uuid", enumerator_num, hh_location_wb, gps.governorate, oslo_area, gps.oslo.area) %>% 
  dplyr::mutate(issue=case_when(
    hh_location_wb!=gps.governorate & oslo_area!=gps.oslo.area ~ issue1,
    hh_location_wb!=gps.governorate ~ issue2,
    oslo_area!=gps.oslo.area ~ issue3))

###########################################################################################################
# [GPS] ADD DISTANCE TO NEAREST SETTLEMEMT
###########################################################################################################
# calculate distance
settlements.idx <- st_nearest_feature(df_wb_sf, settlements)
settlements.distance <- st_distance(df_wb_sf, settlements[settlements.idx,], by_element=TRUE)
df_wb_sf$distance.to.nearest.settlement <- settlements.distance
# add the column to the main dataset
df <- left_join(df, dplyr::select(st_drop_geometry(df_wb_sf), "_uuid", "distance.to.nearest.settlement"), by="_uuid")

###########################################################################################################
# [GPS] Generate map with all samples
###########################################################################################################
map <- leaflet() %>% 
  addPolygons(data=gov_WB, color = "#0080FF", weight = 1, fillOpacity=0.2, opacity = 0.8,
              label = gov_WB$GOVERNORAT) %>%
  addPolygons(data=settlements, color = "#00FFFF", weight = 1, fillOpacity=0.2, opacity = 0.8) %>%
  addCircles(data = df_wb_sf, stroke = F, fill = T, fillOpacity = 1, radius = 50, fillColor="#FF0000",
             label=df_wb_sf$`_index`) %>%
  addMeasure(primaryLengthUnit = "meters") %>% 
  addTiles()
saveWidget(map, file="samples.html")

###########################################################################################################

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



library(dplyr)
library(readxl)
library(Hmisc)



r <- read_excel("input/raw_data/FINAL_Pilot_V12_REACH_oPt_Kobo_MSNA_June_2021_13062021_2021-06-20-05-33-16.xlsx", sheet = "MSNA_I_2021")

##of interviews conducted with head of household
r$hh1 <- ifelse(r$hhh_r == "yes", 1, 0)
r$hh1
table(r$hh1)
prop.table(table(r$hh1))

##% of interviews conducted with male or female participants
r$hh5 <- ifelse(r$gender_respondent == "female", 1, 0)
r$hh5
table(r$hh5)
prop.table(table(r$hh5))

##% of households by sex of household head
r$hh6 <- case_when( r$sex_head_hh == "female" ~ 1,
                    r$sex_head_hh == "male" ~ 0,
                    TRUE ~ NA_real_)
r$hh6
table(r$hh6)
prop.table(table(r$hh6))

##% of interviews conducted according to refugee status of household

r$hh8 <- ifelse(r$refugee_status == "yes", 1, 0)
r$hh8
table(r$hh8)
prop.table(table(r$hh8))

##% of households with at least one member pregnant or lactating

r$hh10 <- ifelse(r$preg_lactating == "yes", 1, 0)
r$hh10 
table(r$hh10)
prop.table(table(r$hh10))

##% of households with at least one member with a chronic disease
r$chronic_illness

r$hh12 <- ifelse(r$chronic_illness == "yes", 1, 0)
table(r$hh12)
prop.table(table(r$hh12))

##% of HHs displaced as a result of the most recent conflict (starting on the 1st of May 2021)
r$permanent_location

r$hhd1 <- case_when(r$permanent_location == "no" ~ 1,
                    r$permanent_location %in% c("yes", "do_not_know", "decline_to_answer") ~ 0,
                    TRUE ~ NA_real_)
r$hhd1
table(r$hhd1)
prop.table(table(r$hhd1))

##% of HHs that have been displaced as a result of the recent conflict, 
##but have since returned to their previous location
r$displacement_status

r$hhd2 <- case_when(r$displacement_status == "yes" ~ 1,
                    r$displacement_status %in% c("no", "do_not_know", "decline_to_answer") ~ 0,
                    TRUE ~ NA_real_)

r$hhd2
table(r$hhd2)
prop.table(table(r$hhd2))

##% of households currently hosting displaced individuals
r$currently_hosting_displaced

r$hhd3 <- case_when(r$currently_hosting_displaced == "yes" ~ 1,
                    r$currently_hosting_displaced %in% c("no", "do_not_know", "decline_to_answer") ~ 0,
                    TRUE ~ NA_real_)
 
r$hhd3 
table(r$hhd3) 
prop.table(table(r$hhd3)) 
 
##Average number of displace individuals hosted in host HHs
r$num_displaced

## households that can access a functional basic and secondary school within a 30min walk from dwellings

r$E11 <- ifelse(r$primary_school_distance %in% c("less_15", "less_30") &
                   r$secondary_school_distance %in% c("less_15", "less_30"),1,0)
table(r$E11)
prop.table(table(r$E11)) ###0.8761062

##HHs whose monthly income has decreased as a result of COVID-19
r$L2 <- ifelse(r$income_change_covid == "yes" ,1,0)

table(r$L2)
prop.table(table(r$L2))

##HHs with debt value > 30000

r$L4 <- ifelse(r$how_much_debt > 30000, 1,0)

table(r$L4)
prop.table(table(r$L4))

## HH with at least one adult (18+) unemployed and seeking work
r$L9 <- ifelse(r$unemployed_adults > 0, 1,0)
r$L9 
table(r$L9)
prop.table(table(r$L9))

##HH reporting members losing jobs permanently or temporarily as a result of the Covid-19 outbreak
r$L12 <- ifelse(r$covid_loss_job == "yes", 1,0)
r$L12
table(r$L12)
prop.table(table(r$L12))

##AD1  ==  % of HHs who have received aid that are reporting that 
## they are unsatisfied with the quantity of the received aid
r$AD1 <- case_when( r$`aid_not_satisfied/quantity_not_enough` == "1" ~ 1,
                    r$`aid_not_satisfied/quantity_not_enough` == "0" ~ 0,
                    TRUE ~ NA_real_)
r$AD1
table(r$AD1)
prop.table(table(r$AD1))

##AD2  ==  % of HHs who are renting an apartment that report being 
##at risk of eviction

r$AD2 <- case_when( r$occupancy_status == "rented" & r$hh_risk_eviction == "yes" ~ 1,
                     r$occupancy_status == "rented" & 
                    r$hh_risk_eviction %in% c ("no", "do_not_know", "decline_to_answer") ~ 0, 
                    TRUE ~ NA_real_)

r$AD2
table(r$AD2)
prop.table(table(r$AD2))
##AD3  ==  % of HHs who have received food aid that reported being unhappy with 
##the quality of the aid that they have received


r$AD3 <- case_when( r$`aid_not_satisfied/quality_not_good` == "1" ~ 1,
                     r$`aid_not_satisfied/quality_not_good` == "0" ~ 0,
                              TRUE ~ NA_real_)
r$AD3 
table(r$AD3 )
prop.table(r$AD3 )


library(dplyr)
library(readxl)
library(Hmisc)



r <- read_excel("input/raw_data/FINAL_Pilot_V12_REACH_oPt_Kobo_MSNA_June_2021_13062021_2021-06-20-05-33-16.xlsx", sheet = "MSNA_I_2021")

##of interviews conducted with head of household
r$hh1 <- ifelse(r$hhh_r == "yes", 1,0)
r$hh1
table(r$hh1)
prop.table(table(r$hh1))

##% of interviews conducted with male or female participants






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

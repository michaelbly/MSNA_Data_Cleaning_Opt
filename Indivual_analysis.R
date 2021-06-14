indiv_df <- read_excel("input/raw_data/Final__V10__REACH_oPt_Kobo_MSNA_May_2021_V8_27052021_-_all_versions_-_False_-_2021-06-07-08-58-28.xlsx", sheet = "member")

indiv_df <- indiv_df %>%
  mutate('_uuid' = '_submission__uuid')
#recode difficulties to 1 and 0

difficulty <- c("a_lot_of_difficulty","cannot_do_at_all")
no_difficulty <- c("no_difficulty", "some_difficulty")
indiv_df <- indiv_df %>%
  mutate(seeing_difficulty = case_when(difficulty_seeing %in% difficulty ~ 1,
                                       difficulty_seeing %in% no_difficulty ~0,
                                       TRUE~ NA_real_),
         hearing_difficulty = case_when(difficulty_hearing %in% difficulty ~ 1,
                                        difficulty_hearing %in% no_difficulty ~0,
                                        TRUE~ NA_real_),
         walking_difficulty = case_when(difficulty_walking %in% difficulty ~ 1,
                                        difficulty_walking %in% no_difficulty ~0,
                                        TRUE~ NA_real_),
         remembering_difficulty = case_when(difficulty_remembering %in% difficulty ~ 1,
                                            difficulty_remembering %in% no_difficulty ~0,
                                            TRUE~ NA_real_),
         washing_difficulty = case_when(difficulty_washing %in% difficulty ~ 1,
                                        difficulty_washing %in% no_difficulty ~0,
                                        TRUE~ NA_real_),
         communicating_difficulty = case_when(difficulty_communicating %in% difficulty ~ 1,
                                              difficulty_communicating %in% no_difficulty ~0,
                                              TRUE~ NA_real_),
         uuid = '_submission__uuid')

# group indiv data and join with hh level
indiv_analysis <- indiv_df %>% 
  group_by(`_submission__uuid`) %>% 
  summarise(count_seeing_difficulty = sum(seeing_difficulty,na.rm = T),
            count_hearing_difficulty = sum(hearing_difficulty,na.rm = T),
            count_walking_difficulty = sum(walking_difficulty,na.rm = T),
            count_remembering_difficulty = sum(remembering_difficulty,na.rm = T),
            count_washing_difficulty = sum(washing_difficulty,na.rm = T),
            count_communicating_difficulty = sum(communicating_difficulty,na.rm = T))

##df <- left_join(df, indiv_analysis, by = c("_uuid"="_submission__uuid"))
# install packages required
devtools::install_github(
  "mabafaba/koboquest",
  build_opts = c(), force = T)

devtools::install_github(
  "boukepieter/dclogger",
  build_opts = c(), force = T)

devtools::install_github(
  "mabafaba/kobostandards",
  build_opts = c(), force = T
)

devtools::install_github(
  "mabafaba/xlsformfill",
  build_opts = c(), force = T
)



# questionnaire
questions <- read.csv("input/raw_data/kobo_questions.csv", 
                      stringsAsFactors=T, check.names=T)
colnames(questions)[1] <- "type"


choices <- read.csv("input/raw_data/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=T)
colnames(choices)[1] <- "list_name"

dummy_data <- xlsformfill::xlsform_fill(questions, choices, 1000)


write.csv(dummy_data, sprintf("input/raw_data/msna_dummy_%s.csv",today()), row.names = FALSE)

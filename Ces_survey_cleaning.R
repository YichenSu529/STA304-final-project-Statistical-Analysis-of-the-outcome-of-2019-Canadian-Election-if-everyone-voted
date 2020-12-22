#install.packages("opendatatoronto")
#install.packages("devtools")
#devtools::install_github("hodgettsp/cesR")
library(cesR)

get_ces("ces2019_web")
library(haven)
library(tidyverse)
library(labelled)
library(dplyr)

#as factor and slected variables
ces2019_data <- ces2019_web
ces_reduced <- ces2019_data %>%
  select(cps19_gender,
         cps19_age,
         cps19_marital,
         cps19_v_likely,
         cps19_education,
         cps19_income_number,
         cps19_volunteer,
         cps19_interest_gen_1,
         cps19_votechoice,
         cps19_vote_unlikely)

ces_reduced <- ces_reduced %>%
  rename(sex = cps19_gender,
         age = cps19_age,
         marital_status = cps19_marital,
         likely_to_vote = cps19_v_likely,
         edu_level = cps19_education,
         household_income = cps19_income_number,
         participate_volunteer = cps19_volunteer,
         interest_politic = cps19_interest_gen_1,
         votechoice = cps19_votechoice,
         unlikely_vote = cps19_vote_unlikely)

ces_reduced <- ces_reduced %>%
  mutate(vote_liberal = if_else(votechoice == 1, 1, 0))

ces_reduced <- ces_reduced %>%
  mutate(vote_conservative = if_else(votechoice == 2, 1, 0))

ces_reduced <- ces_reduced %>%
  mutate(unlikely_vote_con = if_else(unlikely_vote == 2, 1, 0))

ces_reduced <- ces_reduced %>%
  mutate(vote_conservative = case_when(ces_reduced$likely_to_vote == 1 & ces_reduced$vote_conservative == 1 ~ 1,
                                       ces_reduced$likely_to_vote == 2 & ces_reduced$vote_conservative == 1 ~ 1,
                                       ces_reduced$likely_to_vote == 3 & ces_reduced$unlikely_vote_con == 1 ~ 1))
ces_reduced$vote_conservative <- replace(ces_reduced$vote_conservative, is.na(ces_reduced$vote_conservative), 0)


ces_reduced <- ces_reduced %>%
  mutate(unlikely_vote_lib = if_else(unlikely_vote == 1, 1, 0))

ces_reduced <- ces_reduced %>%
  mutate(vote_liberal = case_when(ces_reduced$likely_to_vote == 1 & ces_reduced$vote_liberal == 1 ~ 1,
                                ces_reduced$likely_to_vote == 2 & ces_reduced$vote_liberal == 1 ~ 1,
                                ces_reduced$likely_to_vote == 3 & ces_reduced$unlikely_vote_lib == 1 ~ 1))
ces_reduced$vote_liberal <- replace(ces_reduced$vote_liberal, is.na(ces_reduced$vote_liberal), 0)

ces_reduced <- ces_reduced %>% select(sex, age, marital_status, likely_to_vote, edu_level, household_income, participate_volunteer,
                                      interest_politic, vote_liberal, vote_conservative)

write_csv(ces_reduced, "survey_data.csv")












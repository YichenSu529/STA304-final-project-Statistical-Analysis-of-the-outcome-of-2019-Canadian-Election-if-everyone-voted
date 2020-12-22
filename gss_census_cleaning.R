#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
  ## 1. Go to: http://www.chass.utoronto.ca/
  ## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
  ## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
  ## 4. Continue in English (you're welcome to use the French, but we probably can't
  ## help you too much).
  ## 5. Crtl F GSS, click
  ## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
  ## wave. In particular the General Social Survey on social identity (cycle 27), 
  ## 2013 has some variables on voter participation if you're into that sort of 
  ## thing. You're welcome to pick any year but this code applies to 2017.
  ## 7. Click download
  ## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
  ## 9. Can select all variables by clicking button next to green colored "All". Then continue.
  ## 10. Create the files, download and save
# Check: 
  ## You WILL need to change the raw data name. Search for .csv - line 41
  ## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAoJjQjy.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
raw_data <- to_factor(raw_data)
gss13_reduced <- raw_data %>% 
  select(sex,
         agegr10,
         marstat,
         vbr_25,
         dh1ged,
         incmhsd,
         vcg_320,
         rep_05,
         wfr_510)


# Fix the names
gss13_reduced <- gss13_reduced %>% 
  rename(sex = sex,
         age = agegr10,
         marital_status = marstat,
         likely_to_vote = vbr_25,
         edu_level = dh1ged,
         household_income = incmhsd,
         participate_volunteer = vcg_320,
         interest_politic = rep_05,
         life_satisfication = wfr_510) 

#### Clean up ####
gss13_reduced <- gss13_reduced %>%
  filter(!is.na(sex)) %>%
  mutate(sex = if_else(sex == 1, "Male", "Female"))


gss13_reduced <- gss13_reduced %>%
  filter(!is.na(age)) %>%
  mutate(age = case_when(
  age == 1 ~ "15 to 34 years",
  age == 2 ~ "15 to 34 years",
  age == 3 ~ "35 to 54 years",
  age == 4 ~ "35 to 54 years",
  age == 5 ~ "55 to 74 years",
  age == 6 ~ "55 to 74 years",
  age == 7 ~ "75 years and over"
    ))
gss13_reduced <- gss13_reduced %>%
  filter(!is.na(marital_status)) %>%
  mutate(if_married = 
           if_else(marital_status == 1, 1, 0))

gss13_reduced <- gss13_reduced %>%
  filter(!is.na(likely_to_vote)) %>%
  mutate(likely_to_vote = if_else(likely_to_vote == 1, "very likely",
                          if_else(likely_to_vote == 2, "somewhat likely",
                          if_else(likely_to_vote == 3, "somewhat unlikely",
                          if_else(likely_to_vote == 4, "very unlikely", "Don't know or not eligible")))))
gss13_reduced <- gss13_reduced %>%
  filter(!is.na(edu_level)) %>%
  filter(edu_level != 6 & edu_level != 6 & edu_level != 8) %>%
  mutate(edu_level = if_else(edu_level == 1 | edu_level == 2, "equal or lower than highschool",
                     if_else(edu_level == 3, "Post-secondary dipolma","Bacholor's degree or higher")))

gss13_reduced <- gss13_reduced %>%
  mutate(participate_volunteer = if_else(participate_volunteer == 1, "yes",
                                 if_else(participate_volunteer == 2, "no", "not answer")))
  
gss13_reduced <- gss13_reduced %>% 
  filter(interest_politic < 6) %>%
  mutate(interest_politic = if_else(interest_politic == 1, "very interested",
                            if_else(interest_politic == 2, "somewhat interested",
                            if_else(interest_politic == 3, "not very interested","not at all interested"))))


# Post-stratification dataset
# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  gss13_reduced %>% select(sex, age, if_married, likely_to_vote, edu_level, interest_politic, participate_volunteer)

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data <- 
  reduced_data %>%
  count(sex, age, if_married, likely_to_vote, edu_level, interest_politic, participate_volunteer) %>%
  group_by(sex, age, if_married, likely_to_vote, edu_level, interest_politic, participate_volunteer) 



# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")


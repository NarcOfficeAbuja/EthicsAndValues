library(tidyverse)
library(naijR)
library(RSQLite)
library(DBI)

source(here::here("scripts/create/helpers.R"))

reg <-
  read_csv("downloads/SOGP VALUES COURSE 3RD COHORT REGISTRATIONS.csv")

reg <- reg %>%
  setNames(
    c(
      "Timestamp",
      "fname",
      "lname",
      "email",
      "mobile",
      "address",
      "dob",
      "gender",
      "occ",
      "educ",
      "school",
      "how_info",
      "nig_dream",
      "prev_proj",
      "expect",
      "ideas"
    )
  )


reg <- reg %>% 
  mutate(Timestamp = strptime(fixdate(Timestamp), "%m/%d/%Y %H:%M:%S")) %>%
  mutate(Timestamp = as.character(Timestamp)) %>% 
  mutate(dob = as.Date(fixdate(dob), "%m/%d/%Y")) %>% 
  mutate(dob = as.character(dob)) %>% 
  mutate(gender = as_factor(gender)) %>% 
  mutate(educ = as_factor(educ)) %>% 
  mutate(prev_proj = factor(prev_proj, c("Yes", "No"))) %>% 
  mutate(mobile = fix_mobile(mobile))

create_cohort_dbtable(reg, 'registration')
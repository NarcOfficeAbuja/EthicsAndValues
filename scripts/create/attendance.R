suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})
library(naijR)

local({
  source(here("scripts/helpers.R"), local = TRUE)
  
  
  # ---- Registrations ----
  reg <-
    read.csv("downloads/SOGP VALUES COURSE 3RD COHORT REGISTRATIONS.csv",
             na.strings = "")
  
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
        "ideas",
        'attended'
      )
    )
  
  reg <- reg %>%
    filter(!(is.na(fname) | is.na(lname))) %>%
    rownames_to_column(var = 'id') %>%
    mutate(id = as.integer(id)) %>%
    mutate(Timestamp = strptime(fixdate(Timestamp), "%m/%d/%Y %H:%M:%S")) %>%
    mutate(Timestamp = as.character(Timestamp)) %>%
    mutate(across(matches("name$"), toupper)) %>%
    mutate(dob = as.Date(fixdate(dob), "%m/%d/%Y")) %>%
    mutate(dob = as.character(dob)) %>%
    mutate(gender = ifelse(gender == "  ", NA_character_, gender)) %>%
    mutate(prev_proj = ifelse(prev_proj == "Yes, No", NA_character_, prev_proj)) %>%
    mutate(mobile = fix_mobile(mobile))
  
  
  ## ---- Attendance ----
  file <- here("downloads/SOGP-VC3C-ATTENDANCE-2.csv")
  dat <- read.csv(file, na.strings = "")
  
  df <- dat %>%
    select(where( ~ !all(is.na(.x)))) %>%
    rename(name = FULL.NAMES) %>%
    filter(!is.na(name)) %>%
    mutate(across(matches("^VRD"), ~ ifelse(. == "P", 1L, 0L)))
  
  newdt <- df %>%
    rownames_to_column(var = "id") %>%
    mutate(
      name = name %>%
        str_replace_all("\\.", " ") %>%
        str_trim %>%
        str_replace("(^\\w+\\s)(\\w\\s)(.+)", "\\1\\3") %>%
        str_remove("\\s\\w$") %>%
        str_trim %>%
        str_squish
    ) %>%
    separate(name, c("first_name", "last_name"), sep = " ")
  
  names.only <- select(newdt, 1:4)
  
  ## Get modules table
  mod <- read_cohort_dbtable('modules') %>%
    select(!module_name)
  tbl <- newdt %>%
    select(!matches('name|course', ignore.case = TRUE)) %>%
    rename(sId = id) %>%
    pivot_longer(matches("^vRD"),
                 names_to = "module_code",
                 values_to = "attended") %>%
    mutate(sId = as.integer(sId)) %>%
    mutate(module_code = str_replace(module_code, "\\.", " ")) %>%
    left_join(mod, by = "module_code") %>%
    select(!module_code) %>%
    rename(mId = id) %>%
    relocate(mId)
  
  
  ## ---- Create the tables ----
  mapply(
    create_cohort_dbtable,
    list(reg, names.only, tbl),
    c("registration", "students", "attendance"),
    MoreArgs = list(overwrite = TRUE)
  )
})
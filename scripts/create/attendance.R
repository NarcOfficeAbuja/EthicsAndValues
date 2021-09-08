suppressPackageStartupMessages(library(tidyverse))
library(here)

source(here("scripts/helpers.R"))

file <- here("downloads/SOGP VC3C ATTENDANCE (Autosaved).xlsx - Sheet1.csv")
dat <- read.csv(file, na.strings = "")

df <- dat %>% 
  rename(name = FULL.NAMES) %>% 
  filter(!is.na(name)) %>% 
  mutate(across(matches("^M"), ~ ifelse(. == "P", 1L, 0L)))

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

names.only <- select(newdt, 1:3)
tbl <- newdt %>% 
  select(!matches('name')) %>% 
  rename(student_id = id) %>%  
  pivot_longer(matches("^M"), names_to = "module_id", values_to = "attended") %>% 
  mutate(module_id = module_id %>% str_remove("^M")) %>% 
  mutate(across(.fns = as.integer))

create_cohort_dbtable(names.only, "students", overwrite = TRUE)
create_cohort_dbtable(tbl, "attendance", overwrite = TRUE)

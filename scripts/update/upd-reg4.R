library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(RSQLite)
library(here)

source(here("scripts/helpers.R"))

reg <- read.csv(here("downloads/registration-cohort-4.csv"))

ureg <- reg %>%
  relocate(GENDER, .after = COUNTRY) %>%
  relocate(DATE.OF.BIRTH, .before = GENDER) %>% 
  setNames(fieldnames('registration')) %>% 
  mutate(attended = ifelse(attended == "YES", 1L, 0L)) %>% 
  mutate(across(all_of(c("location", "country")), ~ str_to_title(.x))) %>% 
  mutate(location = sub("Fct", "FCT", location)) %>% 
  mutate(expect = str_to_sentence(str_to_lower(expect))) %>% 
  filter(!(fname == "" & lname == "")) %>%  # presumably empty rows
  mutate(cohort_id = coht) %>% 
  relocate(cohort_id)

# tryCatch({
#   cat("Writing to database ... ")
#   ccon <- dbConnect(SQLite(), "data/cohort3.db")
#   if (dbIsValid(ccon)) {
#     dbWriteTable(ccon, "registration", ureg, append = TRUE)
#     dbDisconnect(ccon)
#     cat("OK\n")
#   }
# }, error = function(e) { stop(e); cat("Failed\n") })

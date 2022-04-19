library(dplyr, warn.conflicts = FALSE)
library(RSQLite)
library(here)

source(here("scripts/helpers.R"))

reg <- read.csv(here("downloads/registration-cohort-5.csv"))

ureg <- reg %>% 
  select(-c(18:28)) %>% 
  mutate(
    cohort_id = 5L,
    mname = NA_character_,
    Timestamp = NA_character_
  ) %>% 
  rename(
    fname = FIRST.NAME,
    lname = SURNAME,
    email = Email,
    country_code = Telephone.Country.Code..e.g..for.Nigeria...234.,
    mobile = Mobile.Number..Whatsapp.,
    dob = Date.of.Birth,
    gender = Gender,
    location = Town.City,
    country = Country,
    occ = Occupation,
    educ = Educational.Qualification,
    attended = ATTENDED..YES.NO,
    organisation = Name.of.Organisation
  )

# These column names are too long, so we will use indexing to modify them
oldcols <- 13:17
newcols <- names(dat)[16:19]
newcols <- c(newcols, "hindrance")
for (i in seq_along(oldcols)) {
  index <- oldcols[i]
  names(ureg)[index] <- newcols[i]
}


tryCatch({
  cat("Writing to database ... ")
  ccon <- dbConnect(SQLite(), "data/cohort3.db")
  if (dbIsValid(ccon)) {
    dbAppendTable(ccon, "registration", ureg)
    dbDisconnect(ccon)
    cat("OK\n")
  }
}, error = function(e) { stop(e); cat("Failed\n") })

library(dplyr, warn.conflicts = FALSE)
library(RSQLite)
library(here)
library(stringr)

source(here("scripts/helpers.R"))

reg <- read.csv(here("downloads/registration-cohort-5.csv"))

ureg <- reg %>%
  select(-c(18:28)) %>%
  mutate(cohort_id = 5L,
         mname = NA_character_,
         Timestamp = NA_character_) %>%
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
# But, first we collect the names of the fields from the existing table.
tryCatch({
  dcon <- dbConnect(SQLite(), .cohortDbPath())
  fieldnames <- dbListFields(dcon, "registration")
}, finally = dbDisconnect(dcon))

oldcols <- 13:17
newnames <- fieldnames[17:20]
newnames <- c(newnames, "hindrance")
for (i in seq_along(oldcols)) {
  index <- oldcols[i]
  names(ureg)[index] <- newnames[i]
}

# Make shre we have the right data types
final.reg <- ureg %>%
  mutate(
    attended = ifelse(grepl("yes", attended, ignore.case = TRUE), 1L, 0L),
    country = str_replace(country, regex("^Niger.+$", ignore_case = TRUE), "Nigeria"),
    location = str_to_title(str_to_lower(location)) %>% 
      str_replace("Fct|F\\.C\\.T", "FCT"),
    mobile = naijR::fix_mobile(mobile),
    dob = str_to_title(str_to_lower(dob)) %>% 
      str_replace("(\\d+)(st|th|rd|nd)", "\\1") %>% 
      str_remove("^\\d{1,2}\\s\\w+$") %>% 
      str_remove("12 Of June") %>% 
      str_replace("(Sep)(t\\.)", "\\1"),
    dob = lubridate::parse_date_time(
      dob,
      c(
        "%d/%m/%Y",
        "%d-%b-%y",
        "%d-%B-%y",
        "%d-%m-%Y",
        "%d %B %Y",
        "%d %B, %Y",
        "%d %b %Y",
        "%d.%m.%Y",
        "%d,%m,%Y",
        "%B %d, %Y",
        "%Y"
      )
    )
  )

tryCatch({
  cat("Writing to database ... ")
  ccon <- dbConnect(SQLite(), .cohortDbPath())
  if (dbIsValid(ccon)) {
    old.cohort <- dbGetQuery(ccon, "SELECT DISTINCT cohort_id FROM registration;")
    if (!5 %in% old.cohort)
      dbAppendTable(ccon, "registration", final.reg)
    else
      warning("Data were not appended because data for", 
              " this cohort already exist in the database")
    cat("OK\n")
  }
}, error = function(e) {
  cat("Failed\n")
  stop(e)
}, finally = dbDisconnect(ccon))

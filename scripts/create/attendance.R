# This script is for the initial creation of the following tables:
#  1. registrations
#  2. students
#  3. attendance

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})
library(naijR)

source(here("scripts/helpers.R"))


# ---- Registrations ----
fname <- "SOGP VALUES COURSE 3RD COHORT REGISTRATIONS.csv"

fpath <- file.path("downloads", fname)

if (!file.exists(fpath))
  stop("The file ", sQuote(fpath), " does not exist")

reg <- read.csv(fpath, na.strings = "")

nreg <- reg %>%
  mutate(country_code = NA_character_,
         mname = NA_character_,
         country = NA_character_) %>%
  relocate(country_code, .after = Email.address) %>%
  relocate(mname, .after = First.name) %>%
  relocate(country, .after = Residential.Address) %>%
  select(-Name.of.Institution.of.Higher.learning) %>%
  relocate(Have.you.been.involved.in.any.social.reforms.campaign..project.,
           .after = How.did.you.get.to.know.about.the.Values.Course.) %>%
  mutate(name_proj = NA_character_) %>%
  relocate(name_proj,
           .after = Have.you.been.involved.in.any.social.reforms.campaign..project.) %>%
  select(-c(State.your.Nigerian.dream, Share.your.nation.building.ideas)) %>%
  mutate(Attended = ifelse(Attended == "yes", 1L, 0L)) %>%
  setNames(fieldnames("registration"))

mreg <- nreg %>%
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
  mutate(mobile = fix_mobile(mobile)) %>% 
  mutate(cohort_id = 3L) %>% 
  relocate(cohort_id, .after = id)

## ---- Attendance ----
file <- here("downloads/SOGP-VC3C-ATTENDANCE-2.csv")
dat <- read.csv(file, na.strings = "")

df <- dat %>%
  select(where(~ !all(is.na(.x)))) %>%
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

tblx <- newdt %>%
  select(!matches('name|course', ignore.case = TRUE)) %>%
  rename(sId = id) %>%
  pivot_longer(matches("^vRD"),
               names_to = "module_code",
               values_to = "attended") %>%
  mutate(sId = as.integer(sId))

## Convert module codes (2022-04-07)
for (i in seq_along(mod$module_code)) {
  v <- tblx$module_code
  mi <- grep(sprintf("%d$", i), v)
  tblx$module_code <- replace(v, mi, mod$module_code[[i]])
}

tbl <- tblx %>% 
  left_join(mod, by = "module_code") %>%
  select(!module_code) %>%
  rename(mId = id) %>%
  relocate(mId)


## ---- Create the tables ----
# Create registration table separately because of 
# how we want to handle the autoincrement 'id' field
qry <- 'CREATE TABLE "registration" (
	"id"	INTEGER,
	"cohort_id"	INTEGER,
	"Timestamp"	TEXT,
	"fname"	TEXT,
	"mname"	TEXT,
	"lname"	TEXT,
	"email"	TEXT,
	"country_code"	TEXT,
	"mobile"	TEXT,
	"location"	TEXT,
	"country"	TEXT,
	"dob"	TEXT,
	"gender"	TEXT,
	"occ"	TEXT,
	"educ"	TEXT,
	"how_info"	TEXT,
	"prev_proj"	TEXT,
	"name_proj"	TEXT,
	"expect"	TEXT,
	"attended"	INTEGER,
	PRIMARY KEY("id" AUTOINCREMENT)
);'

tryCatch({
  tblname <- "registration"
  con <- dbConnect(SQLite(), here::here("data/cohort3.db"))
  if (dbExistsTable(con, tblname))
    dbSendStatement(con, paste0("DROP TABLE ", tblname, ";"))
  dbSendStatement(con, qry)
  dbWriteTable(con, tblname, mreg, append = TRUE)
  dbDisconnect(con)
}, error = function(e) stop(e))

# Then we create the other tables, 'students' and 'attendance'
mapply(
  create_cohort_dbtable,
  list(names.only, tbl),
  c("students", "attendance"),
  MoreArgs = list(overwrite = TRUE)
)

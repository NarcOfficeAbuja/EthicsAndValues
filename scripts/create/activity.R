library(here)

source(here("scripts/helpers.R"))

dat <- read.csv(here("downloads/events.csv"))

dat$dates <- as.character(as.Date(dat$dates, format = "%m/%d/%Y"))

create_cohort_dbtable(dat, "events")

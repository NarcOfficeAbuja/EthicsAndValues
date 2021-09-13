suppressPackageStartupMessages(library(here))

local({
  source(here("scripts/helpers.R"), local = TRUE)
  
  dat <- read.csv(here("downloads/events.csv"))
  dat$dates <- as.character(as.Date(dat$dates, format = "%m/%d/%Y"))
  
  create_cohort_dbtable(dat, "events", overwrite = TRUE)
})
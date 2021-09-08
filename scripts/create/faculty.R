library(here)
library(naijR)

source(here("scripts/helpers.R"))
dat <- read.csv(here("downloads/faculty.csv"))

dat$phone <- fix_mobile(dat$phone)

create_cohort_dbtable(dat, "faculty")

library(here)

source(here("scripts/helpers.R"))

dat <- read.csv(here("downloads/modules.csv"))
create_cohort_dbtable(dat, "modules")

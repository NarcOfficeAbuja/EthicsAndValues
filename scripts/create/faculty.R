suppressPackageStartupMessages(library(here))
library(naijR)
local({
  source(here("scripts/helpers.R"), local = TRUE)
  dat <- read.csv(here("downloads/faculty.csv"))
  
  dat$phone <- fix_mobile(dat$phone)
  
  create_cohort_dbtable(dat, "faculty", overwrite = TRUE)
})
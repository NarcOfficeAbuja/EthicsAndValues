suppressPackageStartupMessages(library(here))

local({
  source(here("scripts/helpers.R"), local = TRUE)
  
  dat <- read.csv(here("downloads/modules.csv"))
  create_cohort_dbtable(dat, "modules", overwrite = TRUE)
})
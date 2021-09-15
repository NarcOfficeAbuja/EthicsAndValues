# Data on registration
source(here::here("scripts/helpers.R"))

dat <- read_cohort_dbtable("registration")

categorical <- c("gender", "educ", "prev_proj", "attended")

clean_coltypes <-
  function(c, n) {
    # browser()
    if (n %in% categorical)
      c <- factor(c)
    if (n == "Timestamp")
      c <- as.POSIXct(c)
    if (n == "dob")
      c <- as.Date(c)
    c
  }

updat <-
  Map(clean_coltypes, dat, names(dat)) |>
  as.data.frame()
str(updat)

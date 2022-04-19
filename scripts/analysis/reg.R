# Data on registration
source(here::here("scripts/helpers.R"))

dat <- read_cohort_dbtable("registration")

clean_coltypes <-
  function(c, n) {
    categorical <- c("gender", "prev_proj", "attended", "country")
    if (n %in% categorical)
      c <- factor(c)
    if (n == "Timestamp") {
      dt <-
        lubridate::parse_date_time(c,
                                   orders = c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M"),
                                   tz = "Africa/Lagos")
      c <- as.POSIXct(dt)
    }
    if (n == "dob")
      c <- as.Date(c)
    c
  }

updat <-
  Map(clean_coltypes, dat, names(dat)) |>
  as.data.frame()
str(updat)

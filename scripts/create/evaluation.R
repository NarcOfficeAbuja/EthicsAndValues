# Create the database
library(RSQLite, quietly = TRUE)
suppressPackageStartupMessages(library(tidyverse))
local({
  source(here::here("scripts/helpers.R"), local = TRUE)
  
  allweeks <-
    list.files('downloads/eval/results', full.names = TRUE) |>
    lapply(\(file) {
      df <- read.csv(file, header = FALSE)
      emptycols <- vapply(df, \(col) all(is.na(col)), logical(1))
      df <- df[, !emptycols]
      if (ncol(df) == 13L)
        return(df)
      df[,-2]
    })
  
  hdr <- newnames <-
    c(
      'time',
      'challenging',
      'ease',
      'too_distant',
      'rating_dur',
      'recomm_dur',
      'recomm_chng',
      'fac_comm',
      'fac_expert',
      'fac_comfort',
      'fulfil_expect',
      'fulfil_explain',
      'comment'
    )
  
  allweeks[[1]] <- allweeks[[1]][-2, ]   # duplicate name spotted
  
  dat <- lapply(allweeks, function(df) {
    if (length(df) != length(hdr))
      stop("Data frame with ",
           ncol(df),
           " columns is mismatched with header")
    df <- df[-1,]    # row has supposed headers or is blank
    setNames(df, hdr)
  }) |>
    (\(x) Reduce(rbind, x))()
  
  
  final <- dat %>%
    as_tibble %>%
    mutate(time = strptime(fixdate(time), "%m/%d/%Y %H:%M:%S")) %>%
    mutate(time = as.character(time)) %>%
    mutate(recomm_dur = str_replace(recomm_dur, "yrs", "")) %>%
    mutate(recomm_dur = str_replace(recomm_dur, "90.+minutes", "1 hr 30 mins")) %>%
    mutate(recomm_dur = str_replace(recomm_dur, regex("one", ignore_case = TRUE), "1")) %>%
    mutate(recomm_dur = str_replace(recomm_dur, regex("two", ignore_case = TRUE), "2")) %>%
    mutate(recomm_dur = str_replace(recomm_dur, regex("three", ignore_case = TRUE), '3')) %>%
    mutate(recomm_dur = str_replace(recomm_dur, regex("four", ignore_case = TRUE), '4')) %>%
    mutate(recomm_dur = str_extract(recomm_dur, "\\d")) %>%
    mutate(recomm_dur = as.integer(recomm_dur)) %>%
    filter(!is.na(time))
  
  create_cohort_dbtable(final, "evaluation", overwrite = TRUE)
})
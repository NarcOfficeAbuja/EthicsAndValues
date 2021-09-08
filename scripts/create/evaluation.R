# Create the database
library(RSQLite)
library(tidyverse)

source(here::here("scripts/helpers.R"))

allweeks <- list.files('downloads/eval/results', full.names = TRUE) |>
  sapply(read.csv, header = FALSE, USE.NAMES = TRUE)

hdr <- allweeks$`downloads/eval/Module 02 EF.csv`[1, ]
allweeks$`downloads/eval/Module 02 EF.csv` <- 
  allweeks$`downloads/eval/Module 02 EF.csv`[-1, ]
hdr <- hdr[!is.na(hdr)]
hdr <- hdr[-2]

dat <- lapply(allweeks, function(df) {
  df <- df[, -2]
  df <- Filter(Negate(\(x) all(is.na(x))), df) |>
    as.data.frame()
  if (length(df) != length(hdr))
    stop("Data frame with ", ncol(df), " columns is mismatched with header")
  setNames(df, hdr)
  }) |> 
  (\(x) Reduce(rbind, x))()

newnames <-
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

final <- dat %>%
  setNames(newnames) %>% 
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

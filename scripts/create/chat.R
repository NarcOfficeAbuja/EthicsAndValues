library(stringr)
library(purrr)


chatfiles <-
  list.files(here::here("downloads/chat"), full.names = TRUE)

extract_chat_data <- function(file) {
  txt <- readLines(file)
  time <- txt %>%
    str_extract("^\\d.+\\t") %>%
    str_remove("\\t")
  from <- txt %>%
    str_extract("From.+\\:") %>%
    str_remove("From") %>%
    str_remove("\\:") %>%
    str_trim
  msg <- txt %>%
    str_extract("\\:\\s.+$") %>%
    str_remove("\\:") %>%
    str_trim %>%
    str_squish
  data.frame(time = time, from = from, msg = msg)
}

dat <- map_dfr(chatfiles, extract_chat_data)

create_cohort_dbtable(dat, "chats")

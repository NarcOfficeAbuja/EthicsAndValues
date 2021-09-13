invisible(
  lapply(
    list.files(here::here("scripts/create"), full.names = TRUE),
    source
  )
)
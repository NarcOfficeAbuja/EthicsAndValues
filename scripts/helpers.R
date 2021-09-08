create_cohort_dbtable <- function(df, tblname, ...) {
  require(RSQLite, quietly = TRUE)
  success <- tryCatch({
    con <- dbConnect(SQLite(), "data/cohort3.db")
    on.exit(dbDisconnect(con))
    dbWriteTable(con, tblname, df, ...)
    TRUE
  }, error = function(e)
    FALSE)
  print(success)
}



read_cohort_dbtable <- function(tbl) {
  require(RSQLite, quietly = TRUE)
  tryCatch({
    cat("Connecting to the database ... ")
    connect <- dbConnect(SQLite(), here::here("data/cohort3.db"))
    dat <- dbReadTable(connect, tbl)
    cat("OK\n")
  }, error = function(e)
    cat("Failed\n"),
  finally = dbDisconnect(connect))
  dat
}


fixdate <- function(str) {
  stopifnot(is.character(str))
  sub("^(\\d)/", "0\\1/", str)
} 
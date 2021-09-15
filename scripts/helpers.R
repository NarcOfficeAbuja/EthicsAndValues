create_cohort_dbtable <- function(df, tblname, ...) {
  success <- tryCatch({
    con <- cohort_connect()
    on.exit(RSQLite::dbDisconnect(con))
    RSQLite::dbWriteTable(con, tblname, df, ...)
    TRUE
  }, error = function(e)
    FALSE)
  print(success)
}



read_cohort_dbtable <- function(tbl) {
  tryCatch({
    cat("Connecting to the database ... ")
    connect <- cohort_connect()
    dat <- RSQLite::dbReadTable(connect, tbl)
    cat("OK\n")
  }, error = function(e)
    cat("Failed\n"),
  finally = RSQLite::dbDisconnect(connect))
  dat
}


fixdate <- function(str) {
  stopifnot(is.character(str))
  sub("^(\\d)/", "0\\1/", str) |>
    (\(x) {sub("(\\s\\d{2}\\:\\d{2})$", "\\1:00", x)})()
}




query_cohort <- function(qry) {
  conn <- cohort_connect()
  on.exit(RSQLite::dbDisconnect(conn))
  rs <- RSQLite::dbSendQuery(conn, qry)
  on.exit(RSQLite::dbClearResult(rs))
  RSQLite::dbFetch(rs)
}




cohort_connect <- function() {
  RSQLite::dbConnect(RSQLite::SQLite(), here::here("data/cohort3.db"))
}

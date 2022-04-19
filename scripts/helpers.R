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
  tryCatch({
    rs <- RSQLite::dbSendQuery(conn, qry)
    RSQLite::dbFetch(rs)
  }, 
  error = function(e) stop(e), 
  finally = RSQLite::dbClearResult(rs))
}




cohort_connect <- function() {
  RSQLite::dbConnect(RSQLite::SQLite(), .cohortDbPath())
}




fieldnames <- function(table) {
  stopifnot(is.character(table))
  
  fn <- list(
    registration = c(
      "Timestamp",
      "fname",
      'mname',
      "lname",
      "email",
      'country_code',
      "mobile",
      "organisation",
      "location",
      "country",
      "dob",
      "gender",
      "occ",
      "educ",
      "how_info",
      "prev_proj",
      'name_proj',
      "expect",
      'attended'
    )
  )
  fn[[table]]
}




rewrite_cohort_dbtable <- function(data, tblname) {
  stopifnot(is.data.frame(data), is.character(tblname))
  require(RSQLite)
  dbcon <- dbConnect(SQLite(), .cohortDbPath())
  on.exit(dbDisconnect(dbcon))
  if (!dbIsValid(dbcon))
    stop("Connection to the database failed")
  if (!tblname %in% dbListTables(dbcon))
    stop("There is no table ", tblname, " in the database")
  if (!all(names(data) %in% dbListFields(dbcon, tblname)))
    stop("Cannot overwrite a table with data whose fields differ from original")
  tryCatch({
    cat("Re-writing the database... ")
    dbWriteTable(dbcon, tblname, data, overwrite = TRUE)
    cat("Done\n")
  }, error = function(e) cat("Failed\n"))
}



.cohortDbPath <- function() {
  here::here("data/cohort3.db")
}
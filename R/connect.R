#' Connect to Database
#'
#' @param file A string of the database to connect with.
#' @return A connection.
#' @export
ts_connect_db <- function(file = getOption("tsdbr.file", "ts.db")) {
  check_string(file)
  
  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  conn
}
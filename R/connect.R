#' Connect to Database
#'
#' @inheritParams ts_create_db
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

#' Disconnect from Database
#'
#' @param conn An object of class SQLiteConnection.
#' @export
ts_disconnect_db <- function(conn) {
  if(!inherits(conn, "SQLiteConnection"))
    stop("conn must be an SQLiteConnection", call. = FALSE)
  DBI::dbDisconnect(conn)
}

#' Get Table
#'
#' @param table A string of the table to get.
#' @inheritParams ts_create_db
#' @return A data frame of the table
#' @export
ts_get_table <- function(table, file = getOption("tsdbr.file", "ts.db")) {
  check_string(table)
  conn <- ts_connect_db(file)
  on.exit(ts_disconnect_db(conn))
  
  if(!DBI::dbExistsTable(conn, table))
    stop("table '", table, "' does not exist", call. = FALSE)
  
  table <- DBI::dbReadTable(conn, table)
  rownames(table) <- NULL
  table
}
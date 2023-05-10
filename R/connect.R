#' Connect to Database
#'
#' @inheritParams ts_create_db
#' @return A connection.
#' @export
ts_connect_db <- function(file) {
  check_string(file)
  
  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), file, extended_types = TRUE)
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  conn
}

#' Disconnect from Database
#'
#' @param conn An object of class SQLiteConnection.
#' @export
ts_disconnect_db <- function(conn = getOption("tsdbr.conn", NULL)) {
  check_conn(conn)
  DBI::dbDisconnect(conn)
}

#' Get Table
#'
#' @param table A string of the table to get.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the table
#' @export
ts_get_table <- function(table, conn = getOption("tsdbr.conn", NULL)) {
  check_string(table)
  check_conn(conn)

  if(!DBI::dbExistsTable(conn, table))
    stop("table '", table, "' does not exist", call. = FALSE)
  
  table <- DBI::dbReadTable(conn, table)
  rownames(table) <- NULL
  as_tibble(table)
}
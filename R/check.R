check_conn <- function(conn) {
  if (!inherits(conn, "SQLiteConnection")) {
    stop("conn must be an SQLiteConnection", call. = FALSE)
  }
  conn
}

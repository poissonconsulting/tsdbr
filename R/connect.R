hdb_connect <- function(file) {
  check_string(file)

  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  conn
}

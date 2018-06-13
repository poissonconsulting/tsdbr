connect <- function(file) {
  check_string(file)

  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  conn
}

add <- function(data, table, file) {
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))

  DBI::dbWriteTable(conn, table, data, append = TRUE)
  data
}

get <- function(table, file) {
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))

  DBI::dbReadTable(conn, table)
}

get_utc_offset <- function(file) {
  get("Database", file)$UTC_Offset
}

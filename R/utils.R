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

has_column <- function(data, column) {
  check_string(column)
  column %in% colnames(data)
}

missing_column <- function(data, column) {
  !has_column(data, column)
}

sys_time_utc <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC"
  time <- as.character(time)
  time <- sub("\\sUTC$", "", time)
  time
}

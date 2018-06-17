#' Set Disclaimer
#' 
#' Sets disclaimer.
#' @param disclaimer A string of the disclaimer.
#' @inheritParams ts_create_db
#' @return A string of the old disclaimer.
#' @export
ts_set_disclaimer <- function(
  disclaimer = "THE DATA ARE COPYRIGHTED", 
                              file = getOption("tsdbr.file", "ts.db")) {
  check_string(disclaimer)
  old <- ts_get_disclaimer(file = file)
  
  conn <- ts_connect_db(file)
  on.exit(DBI::dbDisconnect(conn))
  
  DBI::dbGetQuery(conn, 
                  paste0("UPDATE Database 
                         SET Disclaimer = '", disclaimer, "'"))
  invisible(old)
}

#' Get Disclaimer
#' 
#' Gets disclaimer as a string.
#' @inheritParams ts_create_db
#' @return A string of the disclaimer.
#' @export
ts_get_disclaimer <- function(file = getOption("tsdbr.file", "ts.db")) {
  get("Database", file = file)$Disclaimer
}

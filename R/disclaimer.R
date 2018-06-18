#' Set Disclaimer
#' 
#' Sets disclaimer.
#' @param disclaimer A string of the disclaimer.
#' @inheritParams ts_disconnect_db
#' @return A string of the old disclaimer.
#' @export
ts_set_disclaimer <- function(
  disclaimer = "THE DATA ARE COPYRIGHTED", 
                              conn = getOption("tsdbr.conn", NULL)) {
  check_string(disclaimer)
  old <- ts_get_disclaimer(conn = conn)

  DBI::dbGetQuery(conn, 
                  paste0("UPDATE Database 
                         SET Disclaimer = '", disclaimer, "'"))
  invisible(old)
}

#' Get Disclaimer
#' 
#' Gets disclaimer as a string.
#' @inheritParams ts_disconnect_db
#' @return A string of the disclaimer.
#' @export
ts_get_disclaimer <- function(conn = getOption("tsdbr.conn", NULL)) {
  ts_get_table("Database", conn = conn)$Disclaimer
}

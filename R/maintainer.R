#' Set Maintainer
#' 
#' Sets maintainer.
#' @param maintainer A string of the maintainer.
#' @inheritParams ts_disconnect_db
#' @return A string of the old maintainer.
#' @export
ts_set_maintainer <- function(maintainer = ts_sys_user(), 
                              conn = getOption("tsdbr.conn", NULL)) {
  check_string(maintainer)
  old <- ts_get_maintainer(conn = conn)

  DBI::dbGetQuery(conn, 
                  paste0("UPDATE Database 
                         SET Maintainer = '", maintainer, "'"))
  invisible(old)
}

#' Get Maintainer
#' 
#' Gets maintainer as a string.
#' @inheritParams ts_disconnect_db
#' @return A string of the maintainer.
#' @export
ts_get_maintainer <- function(conn = getOption("tsdbr.conn", NULL)) {
  ts_get_table("Database", conn = conn)$Maintainer
}

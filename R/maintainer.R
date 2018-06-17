#' Set Maintainer
#' 
#' Sets maintainer.
#' @param maintainer A string of the maintainer.
#' @inheritParams ts_create_db
#' @return A string of the old maintainer.
#' @export
ts_set_maintainer <- function(maintainer = ts_sys_user(), 
                              file = getOption("tsdbr.file", "ts.db")) {
  check_string(maintainer)
  old <- ts_get_maintainer(file = file)
  
  conn <- ts_connect_db(file)
  on.exit(DBI::dbDisconnect(conn))
  
  DBI::dbGetQuery(conn, 
                  paste0("UPDATE Database 
                         SET Maintainer = '", maintainer, "'"))
  invisible(old)
}

#' Get Maintainer
#' 
#' Gets maintainer as a string.
#' @inheritParams ts_create_db
#' @return A string of the maintainer.
#' @export
ts_get_maintainer <- function(file = getOption("tsdbr.file", "ts.db")) {
  ts_get_table("Database", file = file)$Maintainer
}

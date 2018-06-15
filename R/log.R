#' Get Log Table
#' 
#' Gets Log table as a data frame.
#' @inheritParams ts_create_db
#' @return A data frame of the requested data.
#' @export
ts_get_log <- function(file = getOption("tsdbr.file", "ts.db")) {
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))
  
  data <- DBI::dbGetQuery(conn, "SELECT *
    FROM Log")
  rownames(data) <- NULL
  data
}

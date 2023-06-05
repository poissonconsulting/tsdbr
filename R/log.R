#' Get Log Table
#'
#' Gets Log table as a data frame.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the requested data.
#' @export
ts_get_log <- function(conn = getOption("tsdbr.conn", NULL)) {
  res <- DBI::dbSendStatement(conn, "SELECT * FROM Log")
  data <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  rownames(data) <- NULL
  as_tibble(data)
}

#' Get Log Table
#' 
#' Gets Log table as a data frame.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the requested data.
#' @export
ts_get_log <- function(conn = getOption("tsdbr.conn", NULL)) {
  data <- DBI::dbGetQuery(conn, "SELECT *
    FROM Log")
  rownames(data) <- NULL
  as_tibble(data)
}

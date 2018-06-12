#' Add Parameters
#'
#' @param parameters A data frame of parameters with columns Parameter and Units.
#' @inheritParams hdb_create
#' @return
#' @export
hdb_add_parameters <- function(parameters, file) {
  check_data(parameters, values = list(
    Parameter = "",
    Units = ""),
    nrow = TRUE,
    exclusive = TRUE,
    order = TRUE,
    key = "Parameter")

  conn <- hdb_connect(file)
  on.exit(DBI::dbDisconnect(conn))

  invisible(parameters)
}

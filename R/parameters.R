#' Add Parameters
#'
#' @param parameters A data frame of parameters with columns Parameter and Units.
#' @inheritParams hdb_create
#' @return The imported parameters.
#' @export
hdb_add_parameters <- function(parameters, file) {
  check_data(parameters,
             values = list(Parameter = "",
                           Units = ""),
             key = "Parameter")

  parameters <- parameters[c("Parameter", "Units")]

  add(parameters, "Parameters", file)
}

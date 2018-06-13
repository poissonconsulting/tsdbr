#' Add Parameter
#'
#' @param parameter A string of the parameter name.
#' @param units A string of the units
#' @inheritParams hdb_create
#' @return A data frame of the imported parameters.
#' @export
hdb_add_parameter <- function(parameter, units, file) {
  check_string(parameter)
  check_string(units)

  parameters <- data.frame(Parameter = parameter, Units = units,
                           stringsAsFactors = FALSE)

  hdb_add_parameters(parameters, file)
}

#' Add Parameters
#'
#' @param parameters A data frame of parameters with columns Parameter and Units.
#' @inheritParams hdb_create
#' @return A data frame of the imported parameters.
#' @export
hdb_add_parameters <- function(parameters, file) {
  check_data(parameters,
             values = list(Parameter = "",
                           Units = ""),
             key = "Parameter")

  parameters <- parameters[c("Parameter", "Units")]

  add(parameters, "Parameter", file)
}

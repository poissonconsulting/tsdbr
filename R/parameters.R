#' Add Parameter
#'
#' @param parameter A string of the parameter name.
#' @param units A string of the units
#' @inheritParams ts_disconnect_db
#' @return A data frame of the imported parameters.
#' @export
ts_add_parameter <- function(parameter, units, conn = getOption("tsdbr.conn", NULL)) {
  check_string(parameter)
  check_string(units)
  
  parameters <- data.frame(Parameter = parameter,
                           Units = units,
                           stringsAsFactors = FALSE)
  
  ts_add_parameters(parameters, conn)
}

#' Add Parameters
#'
#' @param parameters A data frame of parameters with columns Parameter and Units.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the imported parameters.
#' @export
ts_add_parameters <- function(parameters, conn = getOption("tsdbr.conn", NULL)) {
  check_data(parameters,
             values = list(Parameter = "",
                           Units = ""),
             key = "Parameter",
             nrow = TRUE)
  
  parameters <- parameters[c("Parameter", "Units")]
  
  add(parameters, "Parameter", conn)
}

#' Get Parameter Table
#' 
#' Gets parameter table as a data frame.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the requested data.
#' @export
ts_get_parameters <- function(conn = getOption("tsdbr.conn", NULL)) {
  ts_get_table("Parameter", conn = conn)
}


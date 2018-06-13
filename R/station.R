#' Add Station
#'
#' @param station A string of the station name.
#' @param parameter A string of the parameter.
#' @param period A string of the period. The possible values are 'year', 'month',
#' 'day', 'hour', 'minute' and 'second'. 
#' @inheritParams ts_create
#' @return A data frame of the imported station.
#' @export
ts_add_station <- function(station, parameter, period, file = getOption("tsdbr.file", "ts.db")) {
  check_string(station)
  check_string(parameter)
  check_string(period)

  stations <- data.frame(Station = station,
                         Parameter = parameter,
                         Period = period,
                         stringsAsFactors = FALSE)
  ts_add_stations(stations, file)
}

#' Add Stations
#'
#' @param stations A data frame of stations with columns Station, Parameter,
#' Period. The optional columns are
#' LowerLimit, UpperLimit, Longitude, Latitude, Organization and
#' StationName.
#' @inheritParams ts_create
#' @return The imported station data.
#' @export
ts_add_stations <- function(stations, file = getOption("tsdbr.file", "ts.db")) {
  check_data(stations,
             values = list(Station  = "",
                           Parameter = "",
                           Period = c("year", "month", "day", "hour", "minute", "second")),
             nrow = TRUE,
             key = "Station")
  
  if(missing_column(stations, "LowerLimit")) {
    stations$LowerLimit <- NA_real_
  } else check_vector(stations$LowerLimit, c(1, NA))
  
  if(missing_column(stations, "UpperLimit")) {
    stations$UpperLimit <- NA_real_
  } else check_vector(stations$UpperLimit, c(1, NA))
  
  if(missing_column(stations, "Longitude")) {
    stations$Longitude <- NA_real_
  } else check_vector(stations$Longitude, c(-180, 180, NA))
  
  if(missing_column(stations, "Latitude")) {
    stations$Latitude <- NA_real_
  } else check_vector(stations$Latitude, c(-90, 90, NA))
  
  if(missing_column(stations, "Organization")) {
    stations$Organization <- NA_character_
  } else check_vector(stations$Organization, c("", NA))
  
  if(missing_column(stations, "StationName")) {
    stations$StationName <- NA_character_
  } else check_vector(stations$StationName, c("", NA))
  
  stations <- stations[c("Station", "Parameter", "Period",
                         "LowerLimit", "UpperLimit", "Longitude", "Latitude",
                         "Organization", "StationName")]
  
  add(stations, "Station", file)
}

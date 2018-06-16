#' Add Station
#'
#' @param station A string of the station name.
#' @param parameter A string of the parameter.
#' @param period A string of the period. The possible values are 'year', 'month',
#' 'day', 'hour', 'minute' and 'second'. 
#' @inheritParams ts_create_db
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
#' LowerLimit, UpperLimit, Longitude, Latitude, Elevation, Organization , StationName, StationID and Comments.
#' @inheritParams ts_create_db
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
  
  if(missing_column(stations, "Elevation")) {
    stations$Elevation <- NA_real_
  } else check_vector(stations$Elevation, c(1, NA))
  
  if(missing_column(stations, "Organization")) {
    stations$Organization <- NA_character_
  } else check_vector(stations$Organization, c("", NA))
  
  if(missing_column(stations, "StationName")) {
    stations$StationName <- NA_character_
  } else check_vector(stations$StationName, c("", NA))
  
  if(missing_column(stations, "StationID")) {
    stations$StationID <- NA_character_
  } else check_vector(stations$StationID, c("", NA))
  
  if(missing_column(stations, "Comments")) {
    stations$Comments <- NA_character_
  } else check_vector(stations$Comments, c("", NA))
  
  stations$CommentsStation <- stations$Comments
  
  stations <- stations[c("Station", "Parameter", "Period",
                         "LowerLimit", "UpperLimit", "Longitude", "Latitude",
                         "Elevation", "Organization", "StationName", "StationID",
                         "CommentsStation")]
  
  add(stations, "Station", file)
}

#' Get Stations Table
#' 
#' Gets stations table as a data frame.
#' @param parameters A character of the parameters to filter by.
#' @param periods A character vector of the periods to filter by.
#' @inheritParams ts_create_db
#' @return A data frame of the requested data.
#' @export
ts_get_stations <- function(
  parameters = ts_get_parameters()$Parameter,
  periods = c("year", "month", "day", "hour", "minute", "second"),
  file = getOption("tsdbr.file", "ts.db")) {
  check_vector(parameters, ts_get_parameters()$Parameter, length = TRUE,
               unique = TRUE)
  check_vector(periods, c("year", "month", "day", "hour", "minute", "second"), 
               length = TRUE, unique = TRUE)
  
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))
  
  data <- DBI::dbGetQuery(conn, paste0("SELECT *
    FROM Station
    WHERE Parameter ", in_commas(parameters),
    "AND Period ", in_commas(periods)))
  rownames(data) <- NULL
  data
}

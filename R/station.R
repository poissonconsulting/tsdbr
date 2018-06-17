#' Add Station
#'
#' @param station A string of the station
#' @param parameter A string of the parameter.
#' @param period A string of the period. The possible values are 'year', 'month',
#' 'day', 'hour', 'minute' and 'second'.
#' @param site A string of the site.
#' @inheritParams ts_create_db
#' @return A data frame of the imported station.
#' @export
ts_add_station <- function(station, parameter, period, site, file = getOption("tsdbr.file", "ts.db")) {
  check_string(station)
  check_string(parameter)
  check_string(period)
  check_string(site)
  
  stations <- data.frame(Station = station,
                         Parameter = parameter,
                         Period = period,
                         Site = site,
                         stringsAsFactors = FALSE)
  ts_add_stations(stations, file)
}

#' Add Stations
#' 
#' @param stations A data frame of stations with columns Station, Parameter,
#' Period and Site. The optional columns are
#' LowerLimit, UpperLimit, Elevation, StationName and Comments.
#' @inheritParams ts_create_db
#' @return The imported station data.
#' @export
ts_add_stations <- function(stations, file = getOption("tsdbr.file", "ts.db")) {
  check_data(stations,
             values = list(Station  = "",
                           Parameter = "",
                           Period = c("year", "month", "day", "hour", "minute", "second"),
                           Site = ""),
             nrow = TRUE,
             key = "Station")
  
  if(missing_column(stations, "LowerLimit")) {
    stations$LowerLimit <- NA_real_
  } else check_vector(stations$LowerLimit, c(1, NA))
  
  if(missing_column(stations, "UpperLimit")) {
    stations$UpperLimit <- NA_real_
  } else check_vector(stations$UpperLimit, c(1, NA))
  
  if(missing_column(stations, "Elevation")) {
    stations$Elevation <- NA_real_
  } else check_vector(stations$Elevation, c(1, NA))
  
  if(missing_column(stations, "StationName")) {
    stations$StationName <- NA_character_
  } else check_vector(stations$StationName, c("", NA))
  
  if(missing_column(stations, "Comments")) {
    stations$Comments <- NA_character_
  } else check_vector(stations$Comments, c("", NA))
  
  stations$CommentsStation <- stations$Comments
  
  stations <- stations[c("Station", "Parameter", "Period", "Site",
                         "Elevation", "LowerLimit", "UpperLimit",
                         "StationName", 
                         "CommentsStation")]
  
  add(stations, "Station", file)
}

#' Get Stations Table
#' 
#' Gets stations table as a data frame.
#' @param parameters A character of the parameters to filter by.
#' @param sites A character of the sites to filter by.
#' @param periods A character vector of the periods to filter by.
#' @inheritParams ts_create_db
#' @return A data frame of the requested data.
#' @export
ts_get_stations <- function(
  parameters = NULL,
  periods = c("year", "month", "day", "hour", "minute", "second"),
  sites = NULL,
  file = getOption("tsdbr.file", "ts.db")) {
  
  conn <- ts_connect_db(file)
  on.exit(ts_disconnect_db(conn))
  
  checkor(check_null(parameters), 
          check_vector(parameters, ts_get_parameters(file = file)$Parameter, 
                       length = TRUE, unique = TRUE))
  
  check_vector(periods, c("year", "month", "day", "hour", "minute", "second"), 
               length = TRUE, unique = TRUE)
  
  checkor(check_null(sites), 
          check_vector(sites, ts_get_sites(file = file)$Site, 
                       length = TRUE, unique = TRUE))
  
  if(is.null(parameters)) parameters <- ts_get_parameters(file = file)$Parameter
  if(is.null(sites)) sites <- ts_get_sites(file = file)$Site
  
  data <- DBI::dbGetQuery(conn, paste0("SELECT *
    FROM Station
    WHERE Parameter ", in_commas(parameters),
    "AND Period ", in_commas(periods),
    "AND Site ", in_commas(sites)))
  rownames(data) <- NULL
  data
}

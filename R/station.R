#' Add Station
#'
#' @param station A string of the station
#' @param parameter A string of the parameter.
#' @param site A string of the site.
#' @param period A string of the period. The possible values are 'year', 'month',
#' 'day', 'hour', 'minute' and 'second'.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the imported station.
#' @export
ts_add_station <- function(station, parameter, site, period, conn = getOption("tsdbr.conn", NULL)) {
  check_string(station)
  check_string(parameter)
  check_string(site)
  check_string(period)
  
  stations <- data.frame(Station = station,
                         Parameter = parameter,
                         Period = period,
                         Site = site,
                         stringsAsFactors = FALSE)
  ts_add_stations(stations, conn)
}

#' Delete Station
#' 
#' Deletes all records associated with a station from the database.
#'
#' @param station A string of the station name.
#' @inheritParams ts_disconnect_db
#' @export
ts_delete_station <- function(station, conn = getOption("tsdbr.conn", NULL)) {
  check_string(station)
  if(!station %in% ts_get_stations(conn = conn)$Station) {
    warning("station '", station, "' does not exist")
    return(invisible())
  }
  DBI::dbGetQuery(conn, paste0("DELETE
    FROM Station
    WHERE Station == '", station, "'"))
  invisible()
}

#' Add Stations
#' 
#' @param stations A data frame of stations with columns Station, Parameter,
#' Site and Period. The optional columns are
#' Depth, LowerLimit, UpperLimit, StationName, StationID and Comments.
#' @inheritParams ts_disconnect_db
#' @return The imported station data.
#' @export
ts_add_stations <- function(stations, conn = getOption("tsdbr.conn", NULL)) {
  check_conn(conn)
  check_data(stations,
             values = list(Station  = "",
                           Parameter = "",
                           Site = "",
                           Period = c("year", "month", "day", "hour", "minute", "second")
             ),
             nrow = TRUE,
             key = "Station")
  
  if(missing_column(stations, "LowerLimit")) {
    stations$LowerLimit <- NA_real_
  } else check_vector(stations$LowerLimit, c(1, NA))
  
  if(missing_column(stations, "UpperLimit")) {
    stations$UpperLimit <- NA_real_
  } else check_vector(stations$UpperLimit, c(1, NA))
  
  if(missing_column(stations, "Depth")) {
    stations$Depth <- NA_real_
  } else check_vector(stations$Depth, c(1, NA))
  
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
  
  stations <- stations[c("Station", "Parameter", "Site", "Period",
                         "Depth", "LowerLimit", "UpperLimit",
                         "StationName", "StationID",
                         "CommentsStation")]
  
  add(stations, "Station", conn)
}

#' Get Stations Table
#' 
#' Gets stations table as a data frame.
#' @param parameters A character of the parameters to filter by.
#' @param sites A character of the sites to filter by.
#' @param periods A character vector of the periods to filter by.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the requested data.
#' @export
ts_get_stations <- function(
  parameters = NULL,
  periods = c("year", "month", "day", "hour", "minute", "second"),
  sites = NULL,
  conn = getOption("tsdbr.conn", NULL)) {
  
  checkor(check_null(parameters), 
          check_vector(parameters, ts_get_parameters(conn = conn)$Parameter, 
                       length = TRUE, unique = TRUE, only = TRUE))
  
  check_vector(periods, ts_get_periods(conn = conn), 
               length = TRUE, unique = TRUE, only = TRUE)
  
  checkor(check_null(sites), 
          check_vector(sites, ts_get_sites(conn = conn)$Site, 
                       length = TRUE, unique = TRUE, only = TRUE))
  
  if(is.null(parameters)) parameters <- ts_get_parameters(conn = conn)$Parameter
  if(is.null(sites)) sites <- ts_get_sites(conn = conn)$Site
  
  data <- DBI::dbGetQuery(conn, paste0("SELECT *
    FROM Station
    WHERE Parameter ", in_commas(parameters),
                                       "AND Period ", in_commas(periods),
                                       "AND Site ", in_commas(sites)))
  rownames(data) <- NULL
  data
}

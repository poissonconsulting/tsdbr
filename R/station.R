#' Add Station
#'
#' @param station A string of the station name.
#' @param parameter A string of the parameter.
#' @inheritParams hdb_create
#' @return A data frame of the imported station.
#' @export
hdb_add_station <- function(station, parameter, file) {
  check_string(station)
  check_string(parameter)

  stations <- data.frame(Station = station,
                         Parameter = parameter,
                         LowerLimit = NA_real_,
                         UpperLimit = NA_real_,
                         Longitude = NA_real_,
                         Latitude = NA_real_,
                         Organization = NA_character_,
                         StationName = NA_character_,
                         stringsAsFactors = FALSE)
  hdb_add_stations(stations, file)
}

#' Add Stations
#'
#' @param stations A data frame of stations with columns Station, Parameter,
#' LowerLimit, UpperLimit, Longitude, Latitude, Organization,
#' StationName.
#' @inheritParams hdb_create
#' @return The imported stations.
#' @export
hdb_add_stations <- function(stations, file) {
  check_data(stations,
             values = list(Station  = "",
                           Parameter = "",
                           LowerLimit = c(1, NA),
                           UpperLimit = c(1, NA),
                           Longitude = c(-180, 180, NA),
                           Latitude = c(-90, 90, NA),
                           Organization = c("", NA),
                           StationName = c("", NA)),
             key = "Station")


  stations <- stations[c("Station", "Parameter",
                         "LowerLimit", "UpperLimit", "Longitude", "Latitude",
                         "Organization", "StationName")]

  add(stations, "Station", file)
}

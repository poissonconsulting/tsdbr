#' Add Stations
#'
#' @param stations A data frame of stations with columns Parameter and Units.
#' @inheritParams hdb_create
#' @return The imported stations.
#' @export
hdb_add_stations <- function(stations, file) {
  check_data(stations,
             values = list(Station  = "",
                           Parameter = "",
                           StartDate = Sys.Date(),
                           EndDate = c(Sys.Date(), NA),
                           LowerLimit = c(1, NA),
                           UpperLimit = c(1, NA),
                           Longitude = c(-180, 180, NA),
                           Latitude = c(-90, 90, NA),
                           Organization = c("", NA),
                           StationName = c("", NA)),
             key = "Station")


  stations <- stations[c("Station", "Parameter", "StartDate", "EndDate",
                             "LowerLimit", "UpperLimit", "Longitude", "Latitude",
                             "Organization", "StationName")]

  add(stations, "Station", file)
}

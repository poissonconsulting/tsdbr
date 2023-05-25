#' Translate Stations
#'
#' Translate stations from or to the codings in Station, StationName and StationID.
#' Its useful if the data are provided with different station codings to the main ones used in the database.
#'
#' @param data A data frame with the column Station of the station coding.
#' @param from A string indicating the coding to translate from.
#' Possible values are 'Station', 'StationName' and 'StationID'.
#' @param to A string indicating the coding to translate to.
#' @param na_rm A flag indicating whether to remove missing stations.
#' Possible values are 'Station', 'StationName' and 'StationID'.
#' @inheritParams ts_disconnect_db
#'
#' @return The translated data
#' @export
ts_translate_stations <- function(data, from = "StationID", to = "Station",
                                  na_rm = TRUE,
                                  conn = getOption("tsdbr.conn", NULL)) {
  check_data(data, values = list(Station = ""))
  chk_vector(from)
  check_values(from, c("Station", "StationName", "StationID"))
  check_dim(from, values = 1)
  chk_vector(to)
  check_values(to, c("Station", "StationName", "StationID"))
  check_dim(to, values = 1)
  chk_not_subset(colnames(data), "..ID")

  if (from == to) {
    return(data)
  }

  stations <- ts_get_stations(conn = conn)

  colnames_data <- colnames(data)

  data$..ID <- 1:nrow(data)
  stations_from <- data[c("Station", "..ID")]
  colnames(stations_from) <- c(from, "..ID")
  stations <- stations[c(from, to)]

  stations <- merge(stations, stations_from, all.y = TRUE, by = from)

  missing <- unique(stations[[from]][is.na(stations[[to]])])
  if (length(missing)) {
    warning("the following stations are unrecognised: ", punctuate(missing, "and"),
      call. = FALSE
    )
  }

  stations <- stations[c(to, "..ID")]
  colnames(stations) <- c("Station", "..ID")
  data$Station <- NULL
  data <- merge(data, stations, by = "..ID")
  data <- data[order(data$..ID), ]
  data <- data[colnames_data]
  if (na_rm) data <- data[!is.na(data$Station), ]

  as_tibble(data)
}

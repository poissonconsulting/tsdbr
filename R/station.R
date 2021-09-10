#' Add Station
#'
#' @param station A string of the station.
#' @param parameter A string of the parameter.
#' @param site A string of the site.
#' @param period A string of the period. The possible values are 'year', 'month',
#' 'day', 'hour', 'minute' and 'second'.
#' @param lower_limit A numeric of the lower limit.
#' @param upper_limit A numeric of the upper limit.
#' @param depth A numeric of the depth.
#' @param station_name A string of the station name.
#' @param station_id A string of the station id.
#' @param comments A string of the station comments.
#'
#' @inheritParams ts_disconnect_db
#' @return A data frame of the imported station.
#' @export
ts_add_station <- function(station, parameter, site, period,
                           lower_limit = NA_real_, upper_limit = NA_real_,
                           depth = NA_real_, station_name = NA_character_,
                           station_id = NA_character_, comments = NA_character_,
                           conn = getOption("tsdbr.conn", NULL)) {
  chk_string(station)
  chk_string(parameter)
  chk_string(site)
  chk_string(period)

  chk_vector(lower_limit)
  check_values(lower_limit, c(1, NA))

  chk_vector(upper_limit)
  check_values(upper_limit, c(1, NA))

  chk_vector(depth)
  check_values(depth, c(1, NA))

  chk_vector(station_name)
  check_values(station_name, c("", NA))

  chk_vector(station_id)
  check_values(station_id, c("", NA))

  chk_vector(comments)
  check_values(comments, c("", NA))

  stations <- data.frame(
    Station = station,
    Parameter = parameter,
    Period = period,
    Site = site,
    LowerLimit = lower_limit,
    UpperLimit = upper_limit,
    Depth = depth,
    StationName = station_name,
    StationID = station_id,
    CommentsStation = comments,
    stringsAsFactors = FALSE
  )
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
  chk_string(station)
  if (!station %in% ts_get_stations(conn = conn)$Station) {
    warning("station '", station, "' does not exist")
    return(invisible())
  }
  DBI::dbExecute(conn, paste0("DELETE
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
    values = list(
      Station = "",
      Parameter = "",
      Site = "",
      Period = c("year", "month", "day", "hour", "minute", "second")
    ),
    nrow = TRUE,
    key = "Station"
  )

  if (missing_column(stations, "LowerLimit")) {
    stations$LowerLimit <- NA_real_
  } else {
    chk_vector(stations$LowerLimit)
    check_values(stations$LowerLimit, c(1, NA))
  }

  if (missing_column(stations, "UpperLimit")) {
    stations$UpperLimit <- NA_real_
  } else {
    chk_vector(stations$UpperLimit)
    check_values(stations$UpperLimit, c(1, NA))
  }

  if (missing_column(stations, "Depth")) {
    stations$Depth <- NA_real_
  } else {
    chk_vector(stations$Depth)
    check_values(stations$Depth, c(1, NA))
  }

  if (missing_column(stations, "StationName")) {
    stations$StationName <- NA_character_
  } else {
    chk_vector(stations$StationName)
    check_values(stations$StationName, c("", NA))
  }

  if (missing_column(stations, "StationID")) {
    stations$StationID <- NA_character_
  } else {
    chk_vector(stations$StationID)
    check_values(stations$StationID, c("", NA))
  }

  if (missing_column(stations, "Comments")) {
    stations$Comments <- NA_character_
  } else {
    chk_vector(stations$Comments)
    check_values(stations$Comments, c("", NA))
  }

  stations$CommentsStation <- stations$Comments

  stations <- stations[c(
    "Station", "Parameter", "Site", "Period",
    "Depth", "LowerLimit", "UpperLimit",
    "StationName", "StationID",
    "CommentsStation"
  )]

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
ts_get_stations <- function(parameters = NULL,
                            periods = c("year", "month", "day", "hour", "minute", "second"),
                            sites = NULL,
                            conn = getOption("tsdbr.conn", NULL)) {
  if(!is.null(parameters)) {
    chk_vector(parameters)
    check_values(parameters, ts_get_parameters(conn = conn)$Parameter)
    check_dim(parameters, values = TRUE)
    chk_unique(parameters)
  }

  chk_vector(periods)
  check_dim(periods, values = TRUE)
  chk_unique(periods)
  check_values(periods, ts_get_periods(conn = conn))

  if(!is.null(sites)) {
    chk_vector(sites)
    check_values(sites, ts_get_sites(conn = conn)$Site)
    check_dim(sites, values = TRUE)
    chk_unique(sites)
  }
  if (is.null(parameters)) parameters <- ts_get_parameters(conn = conn)$Parameter
  if (is.null(sites)) sites <- ts_get_sites(conn = conn)$Site

  data <- DBI::dbGetQuery(conn, paste0(
    "SELECT *
    FROM Station
    WHERE Parameter ", in_commas(parameters),
    "AND Period ", in_commas(periods),
    "AND Site ", in_commas(sites)
  ))
  rownames(data) <- NULL
  as_tibble(data)
}

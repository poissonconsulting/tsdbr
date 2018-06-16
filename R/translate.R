#' Translate Stations
#' 
#' Translate stations from the codings in StationID to those in Station (or vice versa).
#' Its useful if the data are provided with different station codings to those main ones used in the database.
#'
#' @param data A data frame with the column Station of the station to translate.
#' @param to_id A flag indicating whether to translate to or from StationID
#' @inheritParams ts_create_db
#'
#' @return The translated data
#' @export
ts_translate_stations <- function(data, to_id = FALSE, file = getOption("tsdbr.file", "ts.db")) {
  check_data(data, values = list(Station = ""))
  check_flag(to_id)
  stations <- ts_get_stations(file = file)
  if(!nrow(data)) return(data)
  
  stations_from <- data["Station"]
  stations <- stations[c("Station", "StationID")]
  
  if(!to_id) colnames(stations) <- rev(colnames(stations))
  
  stations <- merge(stations, stations_from, all.y = TRUE, by = "Station")
  rm(stations_from)
  data$Station <- stations$StationID
  
  missing <- unique(stations$Station[is.na(stations$StationID)])
  if(length(missing)) {
    warning("the following stations are unrecognised: ", punctuate(missing), 
            call. = FALSE)
  }
  data
}

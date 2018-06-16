ts_translate_stations <- function(data, to_id = FALSE, file = getOption("tsdbr.file", "ts.db")) {
  check_data(data, values = list(Station = ""))
  check_flag(to_id)
  stations <- ts_get_stations(file = file)
  if(!nrow(data)) return(data)
  
  station_from <- data["Station"]
  stations <- stations[c("Station", "StationID")]
  
  if(!to_id) colnames(stations) <- rev(colnames(stations))
  
  stations <- merge(stations, stations_from, all.y = TRUE, by = "Station")
  data$Station <- stations$StationID
  
  missing <- unique(stations$Station[is.na(stations$StationID)])
  if(!length(missing)) {
    warning("the following stations are unrecognised: ", punctuate(missing), 
            call. = FALSE)
  }
  data
}

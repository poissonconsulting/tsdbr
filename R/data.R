#' Add Data
#'
#' @param data A data frame of data with columns Station, DateTime, Recorded.
#' Additional optional columns include Corrected, Status, Comments.
#' @inheritParams ts_disconnect_db
#' @param aggregate A flag indicating whether to aggregate and average multiple values within the same station period.
#' This also has the effect of rounding down single values.
#' @param na_rm A flag indicating whether to remove missing values (if possible) when aggregating.
#' @param resolution A string of the action to take with regard to existing values.
#' Options are 'abort', 'ignore' or 'replace'.
#' @return A data frame of the imported parameters.
#' @export
ts_add_data <- function(data, aggregate = FALSE, na_rm = FALSE,
                        resolution = "abort",
                        conn = getOption("tsdbr.conn", NULL)) {
  check_data(data,
             values = list(Station = "",
                           DateTime = Sys.time(),
                           Recorded = c(1, NA)),
             nrow = TRUE,
             key = c("Station", "DateTime"))
  check_conn(conn)
  
  tz <- get_tz(conn)
  
  if(tz == "GMT") {
    checkor(check_tzone(data$DateTime, "UTC"),check_tzone(data$DateTime, "GMT"))
  } else {
    check_tzone(data$DateTime, tz)
  }
  
  if(missing_column(data, "Corrected")) {
    data$Corrected <- data$Recorded
  } else check_vector(data$Corrected, c(1, NA))
  
  if(missing_column(data, "Status")) {
    data$Status <- ordered("reasonable", status_values())
  } else check_vector(data$Status, ordered(status_values(), status_values()))
  
  if(missing_column(data, "Comments")) {
    data$Comments <- NA_character_
  } else check_vector(data$Comments, c("", NA))
  
  check_flag(aggregate)
  check_flag(na_rm)
  check_vector(resolution, c("abort", "ignore", "replace"), length = 1)
  
  data$DateTimeData <- format(data$DateTime, format = "%Y-%m-%d %H:%M:%S")
  data$CommentsData <- data$Comments
  data$Status <- as.integer(data$Status)
  
  data <- data[c("Station", "DateTimeData", "Recorded",
                 "Corrected", "Status", "CommentsData")]
  
  stations <- ts_get_table("Station", conn)
  
  check_vector(data$Station, stations$Station, only = TRUE)
  
  data <- merge(data, stations[c("Station", "LowerLimit", "UpperLimit")], by = "Station")
  
  data$Status[is.na(data$Corrected)] <- 1L
  data$Status[!is.na(data$Corrected) & !is.na(data$LowerLimit) & data$Corrected < data$LowerLimit] <- 3L
  data$Status[!is.na(data$Corrected) & !is.na(data$UpperLimit) & data$Corrected > data$UpperLimit] <- 3L
  
  data$LowerLimit <- NULL
  data$UpperLimit <- NULL
  
  if(aggregate) {
    data <- merge(data, stations[c("Station", "Period")], by = "Station")
    data <- round_down_time(data)
    data$Period <- NULL
    
    data <- aggregate_time_station(data, na_rm = na_rm) 
  }
  data$UploadedUTC <- sys_time_utc()
  on.exit(DBI::dbGetQuery(conn, "DELETE FROM Upload;"))
  on.exit(DBI::dbGetQuery(conn, "VACUUM;"), add = TRUE)
  DBI::dbGetQuery(conn, "DELETE FROM Upload;")
  
  add(data, "Upload", conn)
  
  period <- DBI::dbGetQuery(conn, "SELECT s.Station As Station, s.Period AS Period, 
      MAX(STRFTIME('%m', u.DateTimeData)) != '01' AS MonthData,
      MAX(STRFTIME('%d', u.DateTimeData)) != '01' AS DayData,
      MAX(STRFTIME('%H', u.DateTimeData)) != '00' AS HourData,
      MAX(STRFTIME('%M', u.DateTimeData)) != '00' AS MinuteData,
      MAX(STRFTIME('%S', u.DateTimeData)) != '00' AS SecondData
    FROM Station s
    INNER JOIN Upload u ON s.Station = u.Station
    GROUP BY s.Station, s.Period
    HAVING 
      (SecondData == 1 AND Period IN ('year', 'month', 'day', 'hour', 'minute')) OR
      (MinuteData == 1 AND Period IN ('year', 'month', 'day', 'hour')) OR
      (HourData == 1 AND Period IN ('year', 'month', 'day')) OR
      (DayData == 1 AND Period IN ('year', 'month')) OR
      (MonthData == 1 AND Period IN ('year'));")
  
  if(nrow(period)) {
    stop("there are ", 
         length(unique(period$Station)), " stations",
         " with date times that are inconsistent with the period", call. = FALSE)
  }
  x <- DBI::dbGetQuery(conn, paste0("INSERT OR ", toupper(resolution), 
                                    " INTO Data SELECT * FROM Upload;"))
  
  DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES('", data$UploadedUTC[1], "',
                               'INSERT', 'Data', '", toupper(resolution), "');"))
  
  invisible(data)
}

#' Get Data
#'
#' @param stations A character vector of the stations.
#' @param start_date The start date.
#' @param end_date The end date.
#' @param period A string of the period to aggregate and average by.
#' The possible values are 'year', 'month', 'day', 'hour', 'minute' and 'second'.
#' @param status A string of the worse type of data to get.
#' The possible values are 'reasonable', 'questionable' or 'erroneous'.
#' @param fill A flag indicating whether to fill in gaps (with missing values).
#' @inheritParams ts_disconnect_db
#' @inheritParams ts_add_data
#' @return A data frame of the requested data.
#' @export
ts_get_data <- function(stations = NULL,
                        start_date = NULL, 
                        end_date = NULL,
                        period = "hour",
                        na_rm = FALSE,
                        status = "questionable",
                        fill = FALSE,
                        conn = getOption("tsdbr.conn", NULL)) {
  checkor(check_null(start_date), check_date(start_date))
  checkor(check_null(end_date), check_date(end_date))
  check_vector(period, ts_get_periods(conn = conn), length = 1, only = TRUE)
  check_vector(status, c("reasonable", "questionable", "erroneous"), length = 1)
  check_flag(fill)
  check_conn(conn)
  
  checkor(check_null(stations), 
          check_vector(stations, ts_get_stations(conn = conn)$Station, 
                       length = TRUE, unique = TRUE, only = TRUE))
  
  if(is.null(stations)) stations <- ts_get_stations(conn = conn)$Station
  
  if(is.null(start_date) || is.null(end_date)) {
    span <- DBI::dbGetQuery(conn, paste0("
    SELECT Station, MIN(DateTimeData) AS Start, MAX(DateTimeData) AS End
    FROM Data
    WHERE Station ", in_commas(stations), "
    GROUP BY Station"))
    
    if(!nrow(span)) {
      return(data.frame(Station = character(0),
                        DateTime = as.POSIXct(character(0), tz = get_tz(conn)),
                        Recorded = numeric(0),
                        Corrected = numeric(0),
                        Status = ts_integer_to_status(integer(0)),
                        Site = character(0),
                        Depth = numeric(0),
                        Parameter = character(0),
                        Units = character(0),
                        StationName = character(0),
                        Comments = character(0),
                        stringsAsFactors = FALSE))
    }

    if(is.null(start_date)) start_date <- min(as.Date(span$Start))
    if(is.null(end_date)) end_date <- max(as.Date(span$End))
  }
  
  if (end_date < start_date) stop("end_date must be after start_date", call. = FALSE)
  
  data <- DBI::dbGetQuery(conn, paste0(
    "SELECT Station, DateTimeData, Recorded, Corrected, Status, CommentsData
    FROM Data
    WHERE Station ", in_commas(stations), " AND
    DATE(DateTimeData) >= '", start_date, "' AND
    DATE(DateTimeData) <= '", end_date, "'
    "))
  
  if(nrow(data)) {  
    if(period != "second") {
      data$Period <- period
      data <- round_down_time(data)
      data$Period <- NULL
      
      data <- aggregate_time_station(data, na_rm = na_rm)
    }
    
    status <- as.integer(ordered(status, status_values()))
    data <- data[data$Status <= status,]
    
  }
  data$Status <- ts_integer_to_status(data$Status)
  
  tz <- get_tz(conn)
  data$DateTimeData <- as.POSIXct(data$DateTimeData, tz = tz)
  
  if(fill) {
    datetimes <- seq(as.POSIXct(as.character(start_date), tz = tz), 
                     as.POSIXct(as.character(end_date), tz = tz),
                     by = period)
    
    all <- expand.grid(Station = stations, DateTimeData = datetimes,
                       stringsAsFactors = FALSE)
    data <- merge(data, all, by = c("Station", "DateTimeData"), all.y = TRUE)
    data$Status[is.na(data$Status)] <- "reasonable"
  }
  
  data <- data[order(data$Station, data$DateTimeData),]
  colnames(data) <- c("Station", "DateTime", "Recorded", "Corrected", "Status",
                      "Comments")
  
  stations <- ts_get_stations(conn = conn)
  parameters <- ts_get_parameters(conn = conn)
  
  parameters <- parameters[c("Parameter", "Units")]
  
  stations <- merge(stations, parameters, by = "Parameter")
  stations <- stations[c("Station", "Site", "Depth", "Parameter", "Units", "StationName")]
  data <- merge(data, stations, by = "Station")
  comments <- data$Comments
  data$Comments <- NULL
  data$Comments <- comments
  rownames(data) <- NULL
  data
}

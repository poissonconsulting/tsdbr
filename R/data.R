#' Add Data
#' 
#' Times are rounded down prior to import.
#'
#' @param data A data frame of data with columns Station, DateTime, Recorded.
#' Additional optional columns include Corrected, Status, Comments.
#' @inheritParams ts_disconnect_db
#' @param aggregate A function to aggregate multiple values within the same station period.
#' @param na_rm A flag indicating whether to remove missing values (if possible) when aggregating.
#' @param resolution A string of the action to take with regard to existing values.
#' Options are 'abort', 'ignore' or 'replace'.
#' @return A data frame of the imported parameters.
#' @export
ts_add_data <- function(data, aggregate = NULL, na_rm = FALSE,
                        resolution = "abort",
                        conn = getOption("tsdbr.conn", NULL)) {
  check_data(data,
             values = list(Station = "",
                           DateTime = Sys.time(),
                           Recorded = c(1, NA)),
             nrow = TRUE)
  
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
  
  checkor(check_null(aggregate), check_function(aggregate))
  check_flag(na_rm)
  check_vector(resolution, c("abort", "ignore", "replace"), length = 1)
  
  # data$DateTimeData <- format(data$DateTime, format = "%Y-%m-%d %H:%M:%S")
  data$DateTimeData <- data$DateTime
  data$CommentsData <- data$Comments
  data$Status <- ts_status_to_integer(data$Status)
  
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
  
  data <- merge(data, stations[c("Station", "Period")], by = "Station")
  data <- round_down_time(data)
  data$Period <- NULL
  
  if(!is.null(aggregate)) {
    data <- aggregate_time_station(data, na_rm = na_rm, aggregate = aggregate)
  } else {
    duplicates <- unique(data[duplicated(data[c("Station", "DateTimeData")]),]$Station)
    if(length(duplicates)) {
      stop("there are ", 
           length(duplicates), " stations",
           " with the same rounded-down date times", call. = FALSE)
    }
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
  
  x <- DBI::dbGetQuery(conn, paste0("INSERT OR ", toupper(resolution), 
                                    " INTO Data SELECT * FROM Upload;"))
  
  DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES('", data$UploadedUTC[1], "',
                               'INSERT', 'Data', '", toupper(resolution), "');"))
  
  invisible(as_tibble(data))
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
                        fill = TRUE,
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
  
  tz <- get_tz(conn)
  
  if(!is.null(start_date)) {
    start_date <- as.numeric(dttr2::dtt_date_time(start_date, tz = tz))
  }
  
  if(!is.null(end_date)) {
    end_date <- as.numeric(dttr2::dtt_date_time(paste(end_date, "23:59:59", sep = " "), tz = tz))
  }
  
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
    
    if(is.null(start_date)) {
      start_date <- min(dttr2::dtt_date(dttr2::dtt_adjust_tz(dttr2::dtt_date_time(span$Start, tz = "UTC"), tz = tz)))
      start_date <- as.numeric(dttr2::dtt_date_time(start_date, tz = tz))
    }
    if(is.null(end_date)) {
      end_date <- max(dttr2::dtt_date(dttr2::dtt_adjust_tz(dttr2::dtt_date_time(span$End, tz = "UTC"), tz = tz)))
      end_date <- as.numeric(dttr2::dtt_date_time(paste(end_date, "23:59:59", sep = " "), tz = tz))
    }
    
  }
  
  if (end_date < start_date) stop("end_date must be after start_date", call. = FALSE)
  
  data <- DBI::dbGetQuery(conn, paste0(
    "SELECT Station, DateTimeData, Recorded, Corrected, Status, CommentsData
    FROM Data
    WHERE Station ", in_commas(stations), " AND
    DateTimeData >= '", start_date, "' AND
    DateTimeData <= '", end_date, "'
    "))
  
  data$DateTimeData <- dttr2::dtt_adjust_tz(data$DateTimeData, tz = tz)
  
  if(nrow(data)) {  
    if(period != "second") {
      data$Period <- period
      data <- round_down_time(data)
      data$Period <- NULL
      
      data <- aggregate_time_station(data, na_rm = na_rm, aggregate = mean)
    }
    status <- as.integer(ordered(status, status_values()))
    data <- data[data$Status <= status,]
  }
  
  data$Status <- ts_integer_to_status(data$Status)
  
  
  if(fill) {
    datetimes <- seq(
      dttr2::dtt_adjust_tz(dttr2::dtt_date_time(start_date), tz = tz), 
      dttr2::dtt_adjust_tz(dttr2::dtt_date_time(end_date), tz = tz),
      by = period
    )
    
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
  
  as_tibble(data)
}

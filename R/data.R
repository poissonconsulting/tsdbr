#' Add Data
#'
#' @param data A data frame of data with columns Station, DateTime, Recorded.
#' Additional optional columns include Corrected, Status, Comments.
#' @inheritParams ts_create_db
#' @param aggregate A flag indicating whether to aggregate and average multiple values within the same station period.
#' This also has the effect of rounding down single values.
#' @param na_rm A flag indicating whether to remove missing values (if possible) when aggregating.
#' @param resolution A string of the action to take with regard to existing values.
#' Options are 'abort', 'ignore' or 'replace'.
#' @return A data frame of the imported parameters.
#' @export
ts_add_data <- function(data, aggregate = FALSE, na_rm = FALSE,
                        resolution = "abort",
                        file = getOption("tsdbr.file", "ts.db")) {
  check_data(data,
             values = list(Station = "",
                           DateTime = Sys.time(),
                           Recorded = c(1, NA)),
             nrow = TRUE,
             key = c("Station", "DateTime"))
  
  tz <- get_tz(file)
  
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
  
  data$DateTimeData <- as.character(data$DateTime)
  data$CommentsData <- data$Comments
  data$Status <- as.integer(data$Status)
  
  data <- data[c("Station", "DateTimeData", "Recorded",
                 "Corrected", "Status", "CommentsData")]
  
  stations <- get("Station", file)
  
  if(any(!unique(data$Station) %in% stations$Station))
    stop("unknown stations", call. = FALSE)
  
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
    
    data <- aggregate_time_add(data, na_rm = na_rm) 
  }
  data$UploadedUTC <- sys_time_utc()
  conn <- connect(file)
  on.exit(DBI::dbGetQuery(conn, "DELETE FROM Upload;"))
  on.exit(DBI::dbGetQuery(conn, "VACUUM;"), add = TRUE)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "DELETE FROM Upload;")
  
  add(data, "Upload", file)
  
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
#' @param fill A flag indicating whether to fill in missing values.
#' @param na_replace A scalar indicating what to use to fill in missing values.
#' @inheritParams ts_create_db
#' @inheritParams ts_add_data
#' @return A data frame of the requested data.
#' @export
ts_get_data <- function(stations = ts_get_stations()$Station,
                        start_date = end_date - 366L, 
                        end_date = Sys.Date(),
                        period = "hour",
                        na_rm = FALSE,
                        status = "questionable",
                        fill = FALSE,
                        na_replace = NA,
                        file = getOption("tsdbr.file", "ts.db")) {
  check_vector(stations, "", length = TRUE, unique = TRUE)
  check_date(start_date)
  check_date(end_date)
  check_vector(period, c("year", "month", "day", "hour", "minute", "second"), length = 1)
  check_vector(status, c("reasonable", "questionable", "erroneous"), length = 1)
  check_flag(fill)
  check_length1(na_replace)
  
  if (end_date < start_date) stop("end_date must be after start_date", call. = FALSE)
  
  if(any(!unique(stations) %in% ts_get_stations()$Station))
    stop("unknown stations", call. = FALSE)
  
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))
  
  data <- DBI::dbGetQuery(conn, paste0("SELECT Station, DateTimeData, Corrected, Status
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
      
      data <- aggregate_time_get(data, na_rm = na_rm)
    }
    
    status <- as.integer(ordered(status, status_values()))
    data <- data[data$Status <= status,]
    
  }
  data$Status <- sub("1", "reasonable", data$Status)
  data$Status <- sub("2", "questionable", data$Status)
  data$Status <- sub("3", "erroneous", data$Status)
  data$Status <- ordered(data$Status, levels = status_values())

  tz <- get_tz(file)
  data$DateTimeData <- as.POSIXct(data$DateTimeData, tz = tz)
  
  if(fill) {
    datetimes <- seq(as.POSIXct(as.character(start_date), tz = tz), 
                     as.POSIXct(as.character(end_date), tz = tz),
                     by = period)
    
    all <- expand.grid(Station = stations, DateTimeData = datetimes,
                       stringsAsFactors = FALSE)
    data <- merge(data, all, by = c("Station", "DateTimeData"), all.y = TRUE)
    data$Corrected[is.na(data$Corrected)] <- as.numeric(na_replace)
    data$Status[is.na(data$Status)] <- "reasonable"
  }

  data <- data[order(data$Station, data$DateTimeData),]
  
  rownames(data) <- NULL
  colnames(data) <- c("Station", "DateTime", "Corrected", "Status")
  data
}

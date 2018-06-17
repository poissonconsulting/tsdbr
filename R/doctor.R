#' Doctor Database
#'
#' @param check_limits A flag indicating whether to check if corrected values outside the lower and upper limits are coded as erroneous.
#' @param check_period A flag indicating whether to check if the periods are valid.
#' @param check_gaps A flag indicating whether to check if there are any gaps in the data given the period.
#' @param fix A flag indicating whether to fix any problems
#' @inheritParams ts_create_db
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(check_limits = TRUE,
                         check_period = TRUE,
                         check_gaps = FALSE,
                         fix = FALSE, 
                         file = getOption("tsdbr.file", "ts.db")) {
  check_flag(check_limits)
  check_flag(check_period)
  check_flag(check_gaps)
  check_flag(fix)
  
  conn <- ts_connect_db(file)
  on.exit(DBI::dbGetQuery(conn, "DELETE FROM Upload;"))
  on.exit(DBI::dbGetQuery(conn, "VACUUM;"), add = TRUE)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  if(check_limits) {
    limits <- DBI::dbGetQuery(
      conn, "SELECT d.Station, d.DateTimeData,
        d.Recorded, d.Corrected, 
        d.CommentsData
      FROM Station s
      INNER JOIN Data d ON s.Station = d.Station
      WHERE (d.Corrected < s.LowerLimit OR d.Corrected > s.UpperLimit)  AND
        d.Status != 3;")
    
    if(nrow(limits)) {
      message("there are ", nrow(limits), " non-erroneous (corrected) values at ", 
              length(unique(limits$Station)), " stations",
              " that are outside the lower and upper limits")
      if(fix) {
        limits$Status <- 3L
        limits <- limits[c("Station", "DateTimeData", "Recorded",
                           "Corrected", "Status", "CommentsData")]      
        limits$UploadedUTC <- sys_time_utc()
        
        DBI::dbGetQuery(conn, "DELETE FROM Upload;")
        add(limits, "Upload", file)
        
        DBI::dbGetQuery(conn, paste0("INSERT OR REPLACE INTO Data SELECT * FROM Upload;"))
        
        DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES('", limits$UploadedUTC[1], "',
                               'UPDATE', 'Data', 'REPLACE fix limits');"))
        limits <- limits[integer(0),]
      }
    } 
    limits <- nrow(limits) > 0
  }
  
  if(check_period) {
    
    period <- DBI::dbGetQuery(conn, "
    SELECT d.Station AS Station, s.Period AS Period,
      MAX(STRFTIME('%m', d.DateTimeData)) != '01' AS MonthData,
      MAX(STRFTIME('%d', d.DateTimeData)) != '01' AS DayData,
      MAX(STRFTIME('%H', d.DateTimeData)) != '00' AS HourData,
      MAX(STRFTIME('%M', d.DateTimeData)) != '00' AS MinuteData,
      MAX(STRFTIME('%S', d.DateTimeData)) != '00' AS SecondData
    FROM Station s
    INNER JOIN Data d ON s.Station = d.Station
    GROUP BY s.Station, s.Period
    HAVING
      (SecondData == 1 AND Period IN ('year', 'month', 'day', 'hour', 'minute')) OR
      (MinuteData == 1 AND Period IN ('year', 'month', 'day', 'hour')) OR
      (HourData == 1 AND Period IN ('year', 'month', 'day')) OR
      (DayData == 1 AND Period IN ('year', 'month')) OR
      (MonthData == 1 AND Period IN ('year'));")

    if(nrow(period)) {
      message("there are ", 
              length(unique(period$Station)), " stations",
              " with date times that are inconsistent with the period")
      
      if(fix) {
        warning("fix period not yet implemented")
      }
    }
    period <- nrow(period) > 0
  }
  
  if(check_gaps) {
    span <- DBI::dbGetQuery(conn,
                            "SELECT s.Station AS Station, s.Period AS Period,
              d.Start AS Start, d.End AS End
              FROM Station AS s INNER JOIN
              DataSpan AS d ON s.Station = d.Station")
    
    span <- split(span, 1:nrow(span))
    span <- lapply(span, FUN = function(x) {
      datetimes <- seq(as.POSIXct(x$Start, tz = "UTC"),
                       as.POSIXct(x$End, tz = "UTC"),
                       by = x$Period)
      datetimes <- format(datetimes, format = "%Y-%m-%d %H:%M:%S")
      data.frame(ID = paste(x$Station, datetimes)) })
    span <- do.call("rbind", span)
    
    data <- DBI::dbGetQuery(
      conn, "SELECT Station || ' ' ||DateTimeData AS ID
              FROM Data")
    
    span <- data.frame(ID = setdiff(span$ID, data$ID))
    rm(data)
    
    span$Station <- sub("(.*)(\\s)(\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}:\\d{2,2})", "\\1", span$ID)
    span$DateTimeData <- sub("(.*)(\\s)(\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}:\\d{2,2})", "\\3", span$ID)
    span$ID <- NULL
    
    if(nrow(span)) {
      message("there are ", nrow(span), " gaps in ",
              nrow(limits), " stations")
      
      if(fix) {
        span$Recorded <- NA_real_
        span$Corrected <- NA_real_
        span$Status <- 1L
        span$CommentsData <- NA_character_
        
        span$UploadedUTC <- sys_time_utc()
        
        DBI::dbGetQuery(conn, "DELETE FROM Upload;")
        add(span, "Upload", file)
        
        DBI::dbGetQuery(conn, paste0("INSERT OR ABORT INTO Data SELECT * FROM Upload;"))
        
        DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES('", span$UploadedUTC[1], "',
                               'INSERT', 'Data', 'ABORT - fix gaps');"))
        span <- span[integer(0),]
      }
    }
    span <- nrow(span) > 0
  }
  !limits & !period & !span
}

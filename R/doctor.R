#' Doctor Database
#'
#' @param check_limits A flag indicating whether to check if corrected values outside the lower and upper limits are coded as erroneous.
#' @param check_period A flag indicating whether to check if the periods are valid.
#' @param check_gaps A flag indicating whether to check if there are any gaps in the data given the period.
#' @param fix A flag indicating whether to fix any problems
#' @inheritParams ts_disconnect_db
#' @return A flag indicating whether or not the database passed the checks (or was fixed)
#' @export
ts_doctor_db <- function(check_limits = TRUE,
                         check_period = FALSE,
                         check_gaps = FALSE,
                         fix = FALSE, 
                         conn = getOption("tsdbr.conn", NULL)) {
  check_flag(check_limits)
  check_flag(check_period)
  check_flag(check_gaps)
  check_flag(fix)
  
  on.exit(DBI::dbGetQuery(conn, "DELETE FROM Upload;"))
  on.exit(DBI::dbGetQuery(conn, "VACUUM;"), add = TRUE)
  
  span <- FALSE
  period <- FALSE
  limits <- FALSE
  
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
      table <- table(limits$Station)
      table <- as.data.frame(table)
      colnames(table) <- c("Station", "Count")
      
      if(fix) {
        limits$Status <- 3L
        limits <- limits[c("Station", "DateTimeData", "Recorded",
                           "Corrected", "Status", "CommentsData")]      
        limits$UploadedUTC <- sys_time_utc()
        
        DBI::dbGetQuery(conn, "DELETE FROM Upload;")
        add(limits, "Upload", conn)
        
        DBI::dbGetQuery(conn, paste0("INSERT OR REPLACE INTO Data SELECT * FROM Upload;"))
        
        DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES('", limits$UploadedUTC[1], "',
                               'UPDATE', 'Data', 'REPLACE fix limits');"))
        limits <- limits[integer(0),]
      }
      message("the following stations ", ifelse(fix, "had", "have"), 
              " non-erroneous (corrected) data", 
              " that are outside the lower and upper limits:\n",
              paste0(utils::capture.output(table), collapse = "\n"))
    } 
    limits <- nrow(limits) > 0
  }
  
  if(check_period) {
    
    warning("check for data period is temporarily disabled")
    
    # period <- DBI::dbGetQuery(conn, "
    #   SELECT d.Station AS Station, s.Period AS Period,
    #     MAX(STRFTIME('%m', d.DateTimeData)) != '01' AS MonthData,
    #     MAX(STRFTIME('%d', d.DateTimeData)) != '01' AS DayData,
    #     MAX(STRFTIME('%H', d.DateTimeData)) != '00' AS HourData,
    #     MAX(STRFTIME('%M', d.DateTimeData)) != '00' AS MinuteData,
    #     MAX(STRFTIME('%S', d.DateTimeData)) != '00' AS SecondData
    #   FROM Station s
    #   INNER JOIN Data d ON s.Station = d.Station
    #   GROUP BY s.Station, s.Period
    #   HAVING
    #     (SecondData == 1 AND Period IN ('year', 'month', 'day', 'hour', 'minute')) OR
    #     (MinuteData == 1 AND Period IN ('year', 'month', 'day', 'hour')) OR
    #     (HourData == 1 AND Period IN ('year', 'month', 'day')) OR
    #     (DayData == 1 AND Period IN ('year', 'month')) OR
    #     (MonthData == 1 AND Period IN ('year'));")
    # 
    # if(nrow(period)) {
    #   if(fix) {
    #     warning("fix period not yet implemented")
    #   }
    #   message("the following stations ", ifelse(FALSE, "had", "have"), 
    #           " date time data that are inconsistent with their periods: ",
    #           punctuate(period$Station, "and"))
    # }
    # 
    # period <- nrow(period) > 0
    
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
      conn, "SELECT Station, DateTimeData FROM Data")
    
    data$DateTimeData <- as.character(data$DateTimeData)
    data$DateTimeData[!grepl(" ", data$DateTimeData)] <- paste(data$DateTimeData[!grepl(" ", data$DateTimeData)], "00:00:00")
    data <- data.frame(ID = paste(data$Station, data$DateTimeData))
    
    span <- data.frame(ID = setdiff(span$ID, data$ID))
    rm(data)
    
    span$Station <- sub("(.*)(\\s)(\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}:\\d{2,2})", "\\1", span$ID)
    span$DateTimeData <- sub("(.*)(\\s)(\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}:\\d{2,2})", "\\3", span$ID)
    span$DateTimeData <- as.numeric(dttr2::dtt_date_time(span$DateTimeData))
    span$ID <- NULL
    
    if(nrow(span)) {
      table <- table(span$Station)
      table <- as.data.frame(table)
      colnames(table) <- c("Station", "Count")
      
      if(fix) {
        span$Recorded <- NA_real_
        span$Corrected <- NA_real_
        span$Status <- 1L
        span$CommentsData <- NA_character_
        span$UploadedUTC <- sys_time_utc()
        
        DBI::dbGetQuery(conn, "DELETE FROM Upload;")
        add(span, "Upload", conn)
        
        DBI::dbGetQuery(conn, paste0("INSERT OR ABORT INTO Data SELECT * FROM Upload;"))
        
        DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES('", span$UploadedUTC[1], "',
                               'INSERT', 'Data', 'ABORT - fix gaps');"))
        span <- span[integer(0),]
      }
      message("the following stations ", ifelse(fix, "had", "have"), 
              " gaps in their data:\n",
              paste0(utils::capture.output(table), collapse = "\n"))
    }
    span <- nrow(span) > 0
  }
  !limits && !span # && !period
}

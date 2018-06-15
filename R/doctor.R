#' Doctor Database
#'
#' @param check_limits A flag indicating whether to check if corrected values outside the lower and upper limits are coded as erroneous.
#' @param check_period A flag indicating whether to check if the periods are valid.
#' @param check_gaps A flag indicating whether to check if there are any gaps in the data given the period.
#' @param fix A flag indicating whether to fix any problems
#' @inheritParams ts_create_db
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
  
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))
  
  DBI::dbGetQuery(conn, "CREATE TEMPORARY VIEW CheckLimitsData AS
      SELECT s.Station AS Station, COUNT(*) AS Outside
      FROM Station s
      INNER JOIN Data d ON s.Station = d.Station
      WHERE (d.Corrected > s.LowerLimit OR d.Corrected > s.UpperLimit)  AND
        d.Status != 3
      GROUP BY s.Station;") # change lowerlimit <
  
  limits <- DBI::dbGetQuery(conn, "SELECT * FROM CheckLimitsData")
  
  if(nrow(limits)) {
    message(sum(limits$Outside), " non-erroneous corrected values at ", 
     nrow(limits), " stations",
     " are outside the lower and upper limits")
  }
  
  DBI::dbGetQuery(conn, "CREATE TEMPORARY VIEW CheckPeriodData AS
    SELECT s.Station As Station, s.Period AS Period, 
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
      (HourData == 1 AND Period IN ('year', 'month', 'day', 'hour')) OR
      (DayData == 1 AND Period IN ('year', 'month')) OR 
      (MonthData == 1 AND Period IN ('year'));") 
  
  # remove hour on hour
  
  period <- DBI::dbGetQuery(conn, "SELECT * FROM CheckPeriodData")
  
  if(nrow(period)) {
    message("the following stations have invalid periods: ", punctuate(period$Station, "and"))
  }
  
  # DBI::dbGetQuery(conn, "CREATE TEMPORARY VIEW CheckPeriodData AS
  #     SELECT s.Station AS Station, s.Period AS Period,
  #     
  #     FROM Station s
  #     INNER JOIN Data d ON s.Station = d.Station
  #     GROUP BY s.Station, s.Period;")
  # 
  # period <- DBI::dbGetQuery(conn, "SELECT * FROM CheckPeriodData")
  # period
}

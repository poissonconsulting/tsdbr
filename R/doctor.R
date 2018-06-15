#' Doctor Database
#'
#' @param check_limits A flag indicating whether to check if corrected values outside the lower and upper limits are coded as erroneous.
#' @param check_period A flag indicating whether to check if 
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
      SELECT s.Station AS Station
      FROM Station s
      INNER JOIN Data d ON s.Station = d.Station
      WHERE (d.Corrected < s.LowerLimit OR d.Corrected > s.UpperLimit)  AND
        d.Status != 3
      GROUP BY s.Station;")
  
  limits <- DBI::dbGetQuery(conn, "SELECT * FROM CheckLimitsData")

  if(nrow(limits))
    message("the following stations",
            " have corrected values outside the lower and upper limits",
            " that are not coded as erroneous:", 
            punctuate(limits$Station, "and"))
  
  NULL
}

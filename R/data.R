#' Add Data
#'
#' @param data A data frame of data with columns Station, DateTime, Recorded.
#' Additional optional columns include Corrected, Status, Comments.
#' @inheritParams ts_create
#' @param resolution A string of the action to take with regard to existing values.
#' Options are 'abort', 'ignore' or 'replace'.
#' @return A data frame of the imported parameters.
#' @export
ts_add_data <- function(data, file, resolution = "abort") {
  check_data(data,
             values = list(Station = "",
                           DateTime = Sys.time(),
                           Recorded = 1),
             nrow = TRUE,
             key = c("Station", "DateTime"))
  
  utc_offset <- get_utc_offset(file)
  
  if(utc_offset == 0L) {
    checkor(check_tzone(data$DateTime, "UTC"),check_tzone(data$DateTime, "GMT"))
  } else {
    tz <- paste0("Etc/GMT", ifelse(utc_offset > 0, "+", "-"), abs(utc_offset))
    check_tzone(data$DateTime, tz)
  }
  
  if(missing_column(data, "Corrected")) {
    data$Corrected <- data$Recorded
  } else check_vector(data$Corrected, 1)
  
  if(missing_column(data, "Status")) {
    data$Status <- "reasonable"
  } else check_vector(data$Status, c("reasonable", "questionable", "erroneous"))
  
if(missing_column(data, "Comments")) {
    data$Comments <- NA_character_
  } else check_vector(data$Comments, c("", NA))
  
  data$CommentsData <- data$Comments
  
  check_vector(resolution, c("abort", "ignore", "replace"), length = 1)
  
  data$DateTimeData <- as.character(data$DateTime)
  
  data$Status <- factor(data$Status,
                        levels = c("reasonable", "questionable", "erroneous"))
  data$Status <- as.integer(data$Status)
  
  data <- data[c("Station", "DateTimeData", "Recorded",
                 "Corrected", "Status", "CommentsData")]
  
  stations <- get("Station", file)
  stations <- stations[c("Station", "LowerLimit", "UpperLimit")]
  data <- merge(data, stations, by = "Station")
  
  data$Status[!is.na(data$Corrected) &
                !is.na(data$LowerLimit) &
                data$Corrected < data$LowerLimit] <- 3L
  
  data$Status[!is.na(data$Corrected) &
                !is.na(data$UpperLimit) &
                data$Corrected > data$UpperLimit] <- 3L
  
  data$LowerLimit <- NULL
  data$UpperLimit <- NULL
  
  conn <- connect(file)
  on.exit(DBI::dbGetQuery(conn, "DELETE FROM Upload;"))
  on.exit(DBI::dbGetQuery(conn, "VACUUM;"), add = TRUE)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  data <- add(data, "Upload", file)
  
  DBI::dbGetQuery(conn, paste0("INSERT OR ", toupper(resolution), " INTO Data SELECT * FROM Upload;"))
  invisible(data)
}

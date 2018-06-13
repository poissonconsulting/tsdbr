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
  
  if(is.null(data$Corrected)) {
    data$Corrected <- data$Recorded
  } else check_vector(data$Corrected, 1)
  
  if(is.null(data$Status)) {
    data$Status <- "Reasonable"
  } else check_vector(data$Status, c("Reasonable", "Questionable", "Erroneous"))
  
  if(is.null(data$Comments)) {
    data$Comments <- NA_character_
  } else check_vector(data$Comments, c("", NA))
  
  check_vector(resolution, c("abort", "ignore", "replace"), length = 1)

  data$DateTimeReading <- as.character(data$DateTime)

  data$Status <- factor(data$Status,
                        levels = c("Reasonable", "Questionable", "Erroneous"))
  data$Status <- as.integer(data$Status)
  
  print(data)
  
  data <- data[c("Station", "DateTimeReading", "Recorded",
                 "Corrected", "Status", "Comments")]

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

  data <- add(data, "Upload", file)

  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))

  DBI::dbGetQuery(conn, paste("INSERT OR", toupper(resolution), "INTO Data SELECT * FROM Upload;"))
  DBI::dbGetQuery(conn, "DELETE FROM Upload;")
  DBI::dbGetQuery(conn, "VACUUM;")
  data
}

#' Add Data
#'
#' @param data A data frame of data with columns Station, Date, Hour, Time, Value.
#' Additional optional columns include Corrected, Status,
#' @inheritParams hdb_create
#' @param resolution A string of the action to take with regard to existing values.
#' Options are abort, ignore or replace.
#' @return A data frame of the imported parameters.
#' @export
hdb_add_data <- function(data, file, resolution = "abort") {
  check_data(data,
             values = list(Station = "",
                           DateReading = Sys.Date(),
                           HourReading = 0:23,
                           Value = 1,
                           Corrected = 1,
                           Status = c("Reasonable", "Questionable", "Erroneous"),
                           Comments = c("", NA)),
             key = c("Station", "DateReading", "HourReading"))

  check_vector(resolution, c("abort", "ignore", "replace"), length = 1)

  data$DateReading <- as.character(data$DateReading)

  data$Status <- factor(data$Status,
                        levels = c("Reasonable", "Questionable", "Erroneous"))
  data$Status <- as.integer(data$Status)

  data <- data[c("Station", "DateReading", "HourReading", "Value",
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

  if (resolution == "abort") {
    DBI::dbGetQuery(conn, "INSERT INTO Data SELECT * FROM Upload;")
  } else if (resolution == "ignore") {
    DBI::dbGetQuery(conn, "INSERT OR IGNORE INTO Data SELECT * FROM Upload;")
  } else
    DBI::dbGetQuery(conn, "INSERT OR REPLACE INTO Data SELECT * FROM Upload;")
  DBI::dbGetQuery(conn, "DELETE FROM Upload;")
  DBI::dbGetQuery(conn, "VACUUM;")
  data
}

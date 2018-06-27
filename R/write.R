#' Write csv
#'
#' @param data The data frame with minimum of columnms Station and DateTime
#' @inheritParams utils::write.csv
#' @return An invisible copy of saved data.
#' @export
ts_write_csv <- function(data, file = "tsdbr.csv") {
  check_data(data,
             values = list(Station = "",
                           DateTime = Sys.time()),
             nrow = TRUE,
             key = c("Station", "DateTime"))
  
  colnames <- colnames(data)
  data$Year <- format(data$DateTime, "%Y")
  data$Month <- format(data$DateTime, "%m")
  data$Day <- format(data$DateTime, "%d")
  data$Hour <- format(data$DateTime, "%H")
  data$Minute <- format(data$DateTime, "%M")
  data$Second <- format(data$DateTime, "%S")
  data$DateTime <- NULL
  colnames <- setdiff(colnames, "DateTime")
  
  data <- data[c("Year", "Month", "Day", "Hour", "Minute", "Second", colnames)]
  utils::write.csv(data, file = file, row.names = FALSE)
  invisible(as_tibble(data))
}
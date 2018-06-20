plot_station <- function(data) {
  gp <- ggplot2::ggplot(data = data,
                        ggplot2::aes_string(x = "DateTime", y = "Value")) +
    ggplot2::geom_line(ggplot2::aes_string(
      group = "Type", linetype = "Type", color = "Status"), alpha = 1/2) +
    ggplot2::ggtitle(data$Station[1]) +
    ggplot2::scale_color_manual(values = c("black", "blue", "red"), drop = FALSE) +
    ggplot2::scale_linetype_manual(values = c("solid", "dotted"), drop = FALSE)
  print(gp)
  NULL
}

#' Plot Data
#'
#' @param data A data frame of the data to plot with minimum of columns Station,
#' DateTime and Recorded and/or Corrected.
#' @export
ts_plot_data <- function(data) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  check_data(data,
             values = list(Station = "",
                           DateTime = Sys.time()),
             nrow = TRUE,
             key = c("Station", "DateTime"))
  
  checkor(check_data(data, list(Recorded = c(1, NA))),
          check_data(data, list(Corrected = c(1, NA))))
  
  if(missing_column(data, "Corrected")) {
    data$Corrected <- data$Recorded
  } else if (missing_column(data, "Recorded")) {
    data$Recorded <- data$Corrected
  }
  
  if(missing_column(data, "Status")) {
    data$Status <- ts_integer_to_status(1L)
  } else check_vector(data$Status, ordered(status_values(), status_values()))
  
  recorded <- data
  data$Value <- data$Corrected
  data$Type <- "Corrected"
  recorded$Value <- recorded$Recorded
  recorded$Type <- "Recorded"
  data <- rbind(data, recorded)
  rm(recorded)
  data$Type <- factor(data$Type, levels = c("Recorded", "Corrected"))
  
  data <- split(data, data["Station"], drop = TRUE)
  lapply(data, plot_station)
  invisible()
}

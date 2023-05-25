plot_station <- function(data) {
  reasonable <- data
  questionable <- data
  erroneous <- data
  rm(data)

  reasonable$Corrected[reasonable$Status != "reasonable"] <- NA
  questionable$Corrected[questionable$Status != "questionable"] <- NA
  erroneous$Corrected[erroneous$Status != "erroneous"] <- NA

  reasonable$Status <- ts_integer_to_status(1L)
  questionable$Status <- ts_integer_to_status(2L)
  erroneous$Status <- ts_integer_to_status(3L)

  data <- rbind(reasonable, questionable, erroneous, stringsAsFactors = FALSE)

  gp <- ggplot2::ggplot(
    data = data,
    ggplot2::aes_string(x = "DateTime", y = "Corrected")
  ) +
    ggplot2::geom_line(ggplot2::aes_string(color = "Status")) +
    ggplot2::scale_color_manual(values = c("black", "blue", "red"), drop = FALSE) +
    ggplot2::ggtitle(data$Station[1]) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Value")

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
      call. = FALSE
    )
  }

  check_data(data,
    values = list(
      Station = "",
      DateTime = Sys.time()
    ),
    nrow = TRUE
  )

  chkor(
    check_data(data, list(Recorded = c(1, NA))),
    check_data(data, list(Corrected = c(1, NA)))
  )

  if (missing_column(data, "Recorded")) {
    data$Recorded <- data$Corrected
  } else if (missing_column(data, "Corrected")) {
    data$Corrected <- data$Recorded
  }

  if (missing_column(data, "Status")) {
    data$Status <- ts_integer_to_status(1L)
  } else {
    chk_vector(data$Status)
    check_values(data$Status, ordered(status_values(), status_values()))
  }


  data <- split(data, data["Station"], drop = TRUE)
  lapply(data, plot_station)
  invisible()
}

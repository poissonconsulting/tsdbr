#' Status to Integer
#'
#' Converts an ordered factor of status values to an integer vector.
#' @param x An ordered factor of status values.
#' @return An integer vector.
#' @export
#' @examples
#' ts_status_to_integer(ordered(
#'   "questionable",
#'   c("reasonable", "questionable", "erroneous")
#' ))
ts_status_to_integer <- function(x) {
  chk_vector(x)
  check_values(x, ordered(status_values(), levels = status_values()))
  as.integer(x)
}

#' Status to Integer
#'
#' Converts an integer vector to an ordered factor of status values.
#' @param x An integer vector.
#' @return An ordered factor of status values.
#' @export
#' @examples
#' ts_integer_to_status(1:3)
ts_integer_to_status <- function(x) {
  chk_vector(x)
  check_values(x, c(1:3))
  x <- status_values()[x]
  ordered(x, levels = status_values())
}

as_tibble <- function(data) {
  rlang::check_installed("tibble")
  data <- tibble::as_tibble(data)
}

add <- function(data, table, conn) {
  DBI::dbWriteTable(conn, table, data, append = TRUE)
  invisible(as_tibble(data))
}

get_utc_offset <- function(conn) {
  ts_get_table("Database", conn)$UTC_Offset
}

get_tz <- function(conn) {
  ts_utc_offset_to_tz(get_utc_offset(conn))
}

has_column <- function(data, column) {
  chk_string(column)
  column %in% colnames(data)
}

missing_column <- function(data, column) {
  !has_column(data, column)
}

in_commas <- function(x) {
  paste0("IN ('", paste0(x, collapse = "','"), "')")
}

aggregate_na_rm <- function(x, fun, na_rm) {
  if (!na_rm) {
    return(fun(x))
  }
  if (all(is.na(x))) {
    return(x[1])
  }
  fun(x[!is.na(x)])
}

round_down_time <- function(data) {
  data$DateTimeData[data$Period == "minute"] <- dttr2::dtt_floor(data$DateTimeData[data$Period == "minute"], units = "minutes")
  data$DateTimeData[data$Period == "hour"] <- dttr2::dtt_floor(data$DateTimeData[data$Period == "hour"], units = "hours")
  data$DateTimeData[data$Period == "day"] <- dttr2::dtt_floor(data$DateTimeData[data$Period == "day"], units = "days")
  data$DateTimeData[data$Period == "month"] <- dttr2::dtt_floor(data$DateTimeData[data$Period == "month"], units = "months")

  data
}

aggregate_time <- function(data, na_rm, aggregate) {
  if (!anyDuplicated(data$DateTimeData)) {
    return(data[c(
      "Station", "DateTimeData", "Recorded", "Corrected", "Status",
      "CommentsData"
    )])
  }
  data <- split(data, data$DateTimeData, drop = TRUE)
  data <- lapply(data, FUN = function(x) {
    data.frame(
      Station = x$Station[1],
      DateTimeData = x$DateTimeData[1],
      Recorded = aggregate_na_rm(x$Recorded, aggregate, na_rm = na_rm),
      Corrected = aggregate_na_rm(x$Corrected, aggregate, na_rm = na_rm),
      Status = max(x$Status),
      CommentsData = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  data <- do.call("rbind", data)
  row.names(data) <- NULL
  data
}

aggregate_time_station <- function(data, na_rm, aggregate) {
  data <- split(data, data["Station"], drop = TRUE)
  data <- lapply(data, aggregate_time, na_rm = na_rm, aggregate = aggregate)
  data <- do.call("rbind", data)
  row.names(data) <- NULL
  data
}

sys_time_utc <- function() {
  x <- Sys.time()
  attr(x, "tzone") <- "UTC"
  as.character(x)
}

status_values <- function() c("reasonable", "questionable", "erroneous")

#' System User
#'
#' Gets a string of the system user.
#'
#' @return A string of the system user.
#' @export
#'
#' @examples
#' ts_sys_user()
ts_sys_user <- function() {
  unname(Sys.info()["user"])
}

punctuate <- function(x, qualifier = "or") {
  chk_string(qualifier)
  if (is.logical(x) || is.integer(x) || is.numeric(x)) {
    x <- as.character(x)
  } else {
    x <- paste0("'", as.character(x), "'")
  }
  if (length(x) == 1) {
    return(x)
  }
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

plural <- function(x, n = 1L, end = "") {
  chk_string(x)
  n <- as.integer(n)
  chk_whole_number(n)
  chk_gte(n)
  chk_integer(n)

  chk_string(end)
  paste0(x, ifelse(n != 1L, "s", ""), end)
}

# this function needs exporting and setting up so that looks up
# permitted periods from Station table SQL
ts_get_periods <- function(conn = getOption("tsdbr.conn", NULL)) {
  c("year", "month", "day", "hour", "minute", "second")
}

# simplified version of check_tzone from checkr to remove dependency
vld_tzone <- function(x, tzone = "UTC", x_name = NULL) {
  tzone <- chk::chk_string(tzone)

  if (is.null(x_name)) {
    x_name <- chk::deparse_backtick_chk(substitute(x))
  }

  attr_tzone <- attr(x, "tzone")
  if (is.null(attr_tzone)) attr_tzone <- "NULL"
  if (tzone != attr_tzone) {
    FALSE
  } else {
    TRUE
  }
}

check_tzone <- function(x, tzone = "UTC", x_name = NULL) {
  if (vld_tzone(x, tzone, x_name)) {
    return(invisible())
  }

  if (is.null(x_name)) {
    x_name <- chk::deparse_backtick_chk(substitute(x))
  }

  attr_tzone <- attr(x, "tzone")
  if (is.null(attr_tzone)) attr_tzone <- "NULL"
  chk::abort_chk(
    x_name, " time zone must be '", tzone, "' (not '", attr_tzone, "')"
  )
}

#' UTC Offset to Time Zone
#'
#' @param utc_offset An integer of the UTC offset.
#' @return The time zone as a string.
#' @export
#' @examples
#' ts_utc_offset_to_tz(-8L)
ts_utc_offset_to_tz <- function(utc_offset) {
  chk_scalar(utc_offset)
  check_values(utc_offset, c(-12L, 14L))

  if (utc_offset == 0L) {
    return("GMT")
  }
  paste0("Etc/GMT", ifelse(utc_offset < 0, "+", "-"), abs(utc_offset))
}

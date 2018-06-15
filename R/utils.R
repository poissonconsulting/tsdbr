connect <- function(file) {
  check_string(file)
  
  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  conn
}

add <- function(data, table, file) {
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))
  
  DBI::dbWriteTable(conn, table, data, append = TRUE)
  invisible(data)
}

get <- function(table, file) {
  conn <- connect(file)
  on.exit(DBI::dbDisconnect(conn))
  
  DBI::dbReadTable(conn, table)
}

get_utc_offset <- function(file) {
  get("Database", file)$UTC_Offset
}

get_tz <- function(file) {
  utc_offset <- get_utc_offset(file)
  if(utc_offset == 0) return("GMT")
  paste0("Etc/GMT", ifelse(utc_offset > 0, "+", "-"), abs(utc_offset))
}

has_column <- function(data, column) {
  check_string(column)
  column %in% colnames(data)
}

missing_column <- function(data, column) {
  !has_column(data, column)
}

in_commas <- function(x) {
  paste0("IN ('", paste0(x, collapse = "','"), "')")
}

average <- function(x, na_rm) {
  if(!na_rm) return(mean(x))
  if(all(is.na(x))) return(x[1])
  mean(x, na.rm = TRUE)
}

round_down_time <- function(data) { 
  data$DateTimeData[data$Period == "minute"] <- 
    sub("(\\d{4,4})-(\\d{2,2})-(\\d{2,2}) (\\d{2,2}):(\\d{2,2}):(\\d{2,2})", 
        "\\1-\\2-\\3 \\4:\\5:00", 
        data$DateTimeData[data$Period == "minute"])
  
  data$DateTimeData[data$Period == "hour"] <- 
    sub("(\\d{4,4})-(\\d{2,2})-(\\d{2,2}) (\\d{2,2}):(\\d{2,2}):(\\d{2,2})", 
        "\\1-\\2-\\3 \\4:00:00", 
        data$DateTimeData[data$Period == "hour"])
  
  data$DateTimeData[data$Period == "day"] <- 
    sub("(\\d{4,4})-(\\d{2,2})-(\\d{2,2}) (\\d{2,2}):(\\d{2,2}):(\\d{2,2})", 
        "\\1-\\2-\\3 00:00:00", 
        data$DateTimeData[data$Period == "day"])
  
  data$DateTimeData[data$Period == "month"] <- 
    sub("(\\d{4,4})-(\\d{2,2})-(\\d{2,2}) (\\d{2,2}):(\\d{2,2}):(\\d{2,2})", 
        "\\1-\\2-01 00:00:00", 
        data$DateTimeData[data$Period == "month"])
  data
}

aggregate_time_add <- function(data, na_rm) {
  data <- split(data, data[c("Station", "DateTimeData")], drop = TRUE)
  data <- lapply(data, FUN = function(x) {
    data.frame(Station = x$Station[1],
               DateTimeData = x$DateTimeData[1],
               Recorded = average(x$Recorded, na_rm = na_rm), 
               Corrected = average(x$Corrected, na_rm = na_rm),
               Status = max(x$Status),
               CommentsData = paste(unique(x$CommentsData), collapse = " ")) })
  data <- do.call("rbind", data)
  row.names(data) <- NULL
  data
}

aggregate_time_get <- function(data, na_rm) {
  data <- split(data, data[c("Station", "DateTimeData")], drop = TRUE)
  data <- lapply(data, FUN = function(x) {
    data.frame(Station = x$Station[1],
               DateTimeData = x$DateTimeData[1],
               Corrected = average(x$Corrected, na_rm = na_rm),
               Status = max(x$Status)) })
  data <- do.call("rbind", data)
  row.names(data) <- NULL
  data
}

status_to_integer <- function(x) {
  x <- factor(x, levels = c("reasonable", "questionable", "erroneous"))
  as.integer(x)
}

sys_time_utc <- function() {
  x <- Sys.time()
  attr(x, "tzone") <- "UTC"
  as.character(x)
}

punctuate <- function(x, qualifier = "or") {
  check_string(qualifier)
  if (is.logical(x) || is.integer(x) || is.numeric(x)) {
    x <- as.character(x)
  } else
    x <- paste0("'", as.character(x), "'")
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

plural <- function(x, n = 1L, end = "") {
  check_string(x)
  n <- check_count(n, coerce = TRUE)
  check_string(end)
  paste0(x, ifelse(n != 1L, "s", ""), end)
}

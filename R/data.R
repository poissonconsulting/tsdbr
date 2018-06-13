#' Add Data
#'
#' @param data A data frame of data with columns Station, DateTime, Recorded.
#' Additional optional columns include Corrected, Status, Comments.
#' @inheritParams ts_create
#' @param aggregate A flag indicating whether to aggregate and average multiple values within the same station period.
#' This also has the effect of rounding down single values.
#' @param resolution A string of the action to take with regard to existing values.
#' Options are 'abort', 'ignore' or 'replace'.
#' @return A data frame of the imported parameters.
#' @export
ts_add_data <- function(data, aggregate = FALSE, resolution = "abort",
                        file = getOption("tsdbr.file", "ts.db")) {
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
  
  data$DateTimeData <- as.character(data$DateTime)
  
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
  
  check_flag(aggregate)
  check_vector(resolution, c("abort", "ignore", "replace"), length = 1)
  
  data$Status <- factor(data$Status,
                        levels = c("reasonable", "questionable", "erroneous"))
  data$Status <- as.integer(data$Status)
  
  data <- data[c("Station", "DateTimeData", "Recorded",
                 "Corrected", "Status", "CommentsData")]
  
  stations <- get("Station", file)
  
  if(any(!unique(data$Station) %in% stations$Station))
    stop("unknown stations", call. = FALSE)
  
  data <- merge(data, stations[c("Station", "LowerLimit", "UpperLimit")], by = "Station")
  
  data$Status[!is.na(data$LowerLimit) & data$Corrected < data$LowerLimit] <- 3L
  data$Status[!is.na(data$UpperLimit) & data$Corrected > data$UpperLimit] <- 3L
  
  data$LowerLimit <- NULL
  data$UpperLimit <- NULL
  
  if(aggregate) {
    data <- merge(data, stations[c("Station", "Period")], by = "Station")
    
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
    
    data$DateTimeData[data$Period == "year"] <- 
      sub("(\\d{4,4})-(\\d{2,2})-(\\d{2,2}) (\\d{2,2}):(\\d{2,2}):(\\d{2,2})", 
          "\\1-01-01 00:00:00", 
          data$DateTimeData[data$Period == "year"])
    
    data$Period <- NULL
    
    data <- split(data, data[c("Station", "DateTimeData")], drop = TRUE)
    data <- lapply(data, FUN = function(x) {
      data.frame(Station = x$Station[1],
                 DateTimeData = x$DateTimeData[1],
                 Recorded = mean(x$Recorded), 
                 Corrected = mean(x$Corrected),
                 Status = max(x$Status),
                 CommentsData = paste(unique(x$CommentsData), collapse = " ")) })
    data <- do.call("rbind", data)
    row.names(data) <- NULL
  }  
  conn <- connect(file)
  on.exit(DBI::dbGetQuery(conn, "DELETE FROM Upload;"))
  on.exit(DBI::dbGetQuery(conn, "VACUUM;"), add = TRUE)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  data <- add(data, "Upload", file)
  
  DBI::dbGetQuery(conn, paste0("INSERT OR ", toupper(resolution), " INTO Data SELECT * FROM Upload;"))
  invisible(data)
}

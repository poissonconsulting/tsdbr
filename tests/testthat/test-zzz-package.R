context("package")

test_that("package", {
  file <- ":memory:"
  file <- "tsdbr.sqlite"
  if(file.exists(file)) unlink(file)
  conn <- ts_create_db(file = file, utc_offset = -8L, periods = c("day", "hour"))
  teardown(ts_disconnect_db(conn))
  options(tsdbr.conn = conn)
  
  expect_error(DBI::dbGetQuery(conn, paste0(
    "INSERT INTO Database VALUES('tsdbr', '0', 'user', 0, 'Disclaimer');")), 
    "only one row permitted!")
  
  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)
  
  expect_identical(parameters, ts_add_parameter("Temp", "degC"))
  
  expect_is(ts_add_site("Mount Doom"), "data.frame")
  
  expect_error(ts_add_station("S1", "Temp", "Mount Doom", "minute"))  
  expect_is(ts_add_station("S1", "Temp", "Mount Doom", "day"), "data.frame")
  
  stations <- data.frame(Station = "S2",
                         Parameter = "Temp",
                         Site = "Lothlorien",
                         Period = "hour",
                         LowerLimit = 0,
                         UpperLimit = 100,
                         StationID = "t2",
                         stringsAsFactors = FALSE)
  
  expect_error(ts_add_stations(stations))
  stations$Site <- "Mount Doom"
  expect_is(ts_add_stations(stations), "data.frame")
  
  data <- data.frame(Station = "S2", DateTime = ISOdate(2000, 9, 1, 0:23),
                     Recorded = 0:23 - 2,
                     StationID = "t2",
                     stringsAsFactors = FALSE)
  
  data$Recorded[4] <- NA
  
  expect_error(ts_add_data(data), 
               "data[$]DateTime time zone must be 'Etc/GMT[+]8' [(]not 'GMT'[)]")
  data$DateTime <- ISOdate(2000, 9, 1, 0:23, tz = "Etc/GMT+8")
  
  data <- data[-5,]
  
  expect_is(ts_add_data(data), "data.frame")
  
  expect_error(ts_add_data(data), "UNIQUE constraint failed: Data.Station, Data.DateTimeData")
  
  data$Recorded <- data$Recorded - 1
  
  expect_is(ts_add_data(data, resolution = "replace"), "data.frame")
  
  data$Station <- "S1"
  expect_error(ts_add_data(data), "there are 1 stations with date times that are inconsistent with the period")
  
  expect_is(ts_add_data(data, aggregate = TRUE), "data.frame")
  
  data <- ts_get_data(stations = "S1")
  expect_is(data, "data.frame")
  expect_identical(ts_get_parameters(), 
                   data.frame(Parameter = "Temp", Units = "degC", stringsAsFactors = FALSE))
  
  expect_identical(nrow(ts_get_stations()), 2L)
  
  expect_identical(ts_get_stations(periods = c("hour"))$Station, "S2")
  
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous")), 24L)
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"))), 21L)
  expect_identical(nrow(ts_get_data(stations = "S1", end_date = as.Date("2000-09-01"))), 1L)
  expect_identical(nrow(ts_get_data()), 0L)
  expect_identical(nrow(ts_get_data(stations = "S1", end_date = as.Date("2000-09-01"), period = "day", fill = TRUE)), 367L)
  expect_equal(ts_get_data(stations = "S2", end_date = as.Date("2000-09-01"), period = "month", fill = TRUE, na_rm = TRUE, na_replace = -10, status = "erroneous")$Corrected, c(rep(-10, 12), 9.227273),
               tolerance = 0.0000001)
  expect_identical(ts_get_data(start_date = as.Date("2001-01-01"), end_date = as.Date("2001-01-02"), period = "hour", fill = TRUE, na_replace = Inf)$Corrected, rep(Inf, 50))
  expect_identical(ts_get_log()$TableLog, c("Database", "Parameter", "Site", "Station", "Station", "Data", "Data", "Data"))
  
  expect_true(ts_doctor_db(check_gaps = TRUE, fix = TRUE))
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous")), 25L)
  expect_identical(ts_set_disclaimer(), 
                   "THE DATA ARE PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND")
  expect_identical(ts_get_disclaimer(), "THE DATA ARE COPYRIGHTED")
  expect_identical(ts_set_maintainer("me"), ts_sys_user())
  expect_identical(ts_get_maintainer(), "me")
  
  data <- ts_get_data(end_date = as.Date("2000-09-01"))
  expect_identical(colnames(data), c("Station", "DateTime", "Recorded", "Corrected",
                                     "Status", "Site", "Depth", "Parameter", "Units", "StationName", "Comments"))
  expect_identical(nrow(data), 22L)
  expect_equal(colnames(ts_write_csv(data, file = tempfile(fileext=".csv"))),
               c("Year", "Month", "Day", "Hour", "Minute", "Second", "Station",
                 "Recorded", "Corrected",
                 "Status", "Site", "Depth", "Parameter", "Units", "StationName", "Comments"))
  
  expect_warning(ts_translate_stations(data), 
                 "the following stations are unrecognised: 'S1' and 'S2'")
  
  expect_warning(ts_translate_stations(data, from = "Station", to = "StationID"), 
                 "the following stations are unrecognised: 'S1'")
  
  expect_identical(nrow(ts_add_station("3S", "Temp", "Mount Doom", "hour")), 1L)
  expect_identical(nrow(ts_get_data("3S", period = "month", fill = TRUE)), 13L)
  
  data <- data.frame(Station = "S2",
                     DateTimeData = c("2000-09-02 00:00:00", # gap on two
                                      "2000-09-02 03:00:00",
                                      "2000-09-02 03:00:01", # extra period
                                      "2000-09-02 03:00:02",                                                             "2000-09-02 03:00:03", # extra period
                                      "2000-09-02 04:00:00",
                                      "2000-09-02 07:00:00",                            
                                      "2000-09-02 08:00:00"),
                     Recorded = NA_real_,
                     Corrected = c(50,50,50,50,50,50,-1,101),
                     Status = 1L,
                     UploadedUTC = "2018-06-19 00:01:00",
                     CommentsData = NA_character_,
                     stringsAsFactors = FALSE)
  
  DBI::dbWriteTable(conn, name = "Data", value = data, row.names = FALSE, append = TRUE)
  
  expect_message(ts_doctor_db(check_period = FALSE), "the following stations have non-erroneous [(]corrected[)] data that are outside the lower and upper limits.*1\\s+S2\\s+2\\s*$")
  expect_message(ts_doctor_db(check_limits = FALSE), "the following stations have date time data that are inconsistent with their periods.*1\\s+S2\\s+3\\s*$")
 
    expect_message(ts_doctor_db(check_gaps = TRUE), "the following stations have gaps in their data.*1\\s+S2\\s+4\\s*$")
    
  expect_false(ts_doctor_db(fix = TRUE))
  expect_true(ts_doctor_db(check_period = FALSE))
  
  ts_delete_station("3S")
  expect_warning(ts_delete_station("3S"), "station '3S' does not exist")
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous")), 25L)
  ts_delete_station("S2")
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous")), 1L)
  
  
})


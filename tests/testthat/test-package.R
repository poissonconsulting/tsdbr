context("package")

test_that("package", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
  file <- "ts.db"
  if(file.exists(file)) unlink(file)
  setup(ts_create_db(file = file, utc_offset = -8L))
  expect_true(file.exists(file))
  teardown(unlink(file))
  conn <- ts_connect_db(file)
  teardown(ts_disconnect(conn))

  expect_error(DBI::dbGetQuery(conn, paste0(
    "INSERT INTO Database VALUES(0, '0', 'user', 'Disclaimer');")), 
    "only one row permitted!")
  
  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)
  
  expect_identical(parameters, ts_add_parameter("Temp", "degC", file = file))
  
  expect_is(ts_add_site("Mount Doom", file = file), "data.frame")
  
  expect_is(ts_add_station("S1", "Temp", "day", "Mount Doom", file = file), "data.frame")
  
  stations <- data.frame(Station = "S2",
                         Parameter = "Temp",
                         Period = "hour",
                         Site = "Lothlorien",
                         LowerLimit = 0,
                         UpperLimit = 100,
                         StationID = "t2",
                         stringsAsFactors = FALSE)
  
  expect_error(ts_add_stations(stations, file = file))
  stations$Site <- "Mount Doom"
  expect_is(ts_add_stations(stations, file = file), "data.frame")
  
  data <- data.frame(Station = "S2", DateTime = ISOdate(2000, 9, 1, 0:23),
                     Recorded = 0:23 - 2,
                     StationID = "t2",
                     stringsAsFactors = FALSE)
  
  data$Recorded[4] <- NA
  
  expect_error(ts_add_data(data, file = file), 
               "data[$]DateTime time zone must be 'Etc/GMT[+]8' [(]not 'GMT'[)]")
  data$DateTime <- ISOdate(2000, 9, 1, 0:23, tz = "Etc/GMT+8")
  
  data <- data[-5,]
  
  expect_is(ts_add_data(data, file = file), "data.frame")
  
  expect_error(ts_add_data(data, file = file), "UNIQUE constraint failed: Data.Station, Data.DateTimeData")
  
  data$Recorded <- data$Recorded - 1
  
  expect_is(ts_add_data(data, file = file, resolution = "replace"), "data.frame")
  
  data$Station <- "S1"
  expect_error(ts_add_data(data, file = file), "there are 1 stations with date times that are inconsistent with the period")
  
  expect_is(ts_add_data(data, file = file, aggregate = TRUE), "data.frame")
  
  data <- ts_get_data(stations = "S1", file = file)
  expect_is(data, "data.frame")
  expect_identical(ts_get_parameters(file = file), 
                   data.frame(Parameter = "Temp", Units = "degC", stringsAsFactors = FALSE))
  
  expect_identical(nrow(ts_get_stations(file = file)), 2L)
  
  expect_identical(ts_get_stations(file = file, periods = c("hour"))$Station, "S2")
  
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous", file = file)), 24L)
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), file = file)), 21L)
  expect_identical(nrow(ts_get_data(stations = "S1", end_date = as.Date("2000-09-01"), file = file)), 1L)
  expect_identical(nrow(ts_get_data(file = file)), 0L)
  expect_identical(nrow(ts_get_data(stations = "S1", file = file, end_date = as.Date("2000-09-01"), period = "day", fill = TRUE)), 367L)
  expect_equal(ts_get_data(stations = "S2", file = file, end_date = as.Date("2000-09-01"), period = "month", fill = TRUE, na_rm = TRUE, na_replace = -10, status = "erroneous")$Corrected, c(rep(-10, 12), 9.227273),
               tolerance = 0.0000001)
  expect_identical(ts_get_data(file = file, start_date = as.Date("2001-01-01"), end_date = as.Date("2001-01-02"), period = "hour", fill = TRUE, na_replace = Inf)$Corrected, rep(Inf, 50))
  expect_identical(ts_get_log(file)$TableLog, c("Database", "Parameter", "Site", "Station", "Station", "Data", "Data", "Data"))
  
  expect_true(ts_doctor_db(check_gaps = TRUE, fix = TRUE))
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous", file = file)), 25L)
  expect_identical(ts_set_disclaimer(file = file), 
                   "THE DATA ARE PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND")
  expect_identical(ts_get_disclaimer(file = file), "THE DATA ARE COPYRIGHTED")
  expect_identical(ts_set_maintainer(file = file, "me"), ts_sys_user())
  expect_identical(ts_get_maintainer(file = file), "me")
  
  data <- ts_get_data(end_date = as.Date("2000-09-01"), file = file)
  csv <- sub("[.]db$", ".csv", file)
  expect_equal(colnames(ts_write_csv(data, file = csv)), 
               c("Year", "Month", "Day", "Hour", "Minute", "Second", "Station",
                 "Corrected", "Status"))
  teardown(unlink(csv))
  
  expect_warning(ts_translate_stations(data, to_name = TRUE), 
                 "the following stations are unrecognised: 'S1'")

  expect_null(ts_plot_data(data))
})
  
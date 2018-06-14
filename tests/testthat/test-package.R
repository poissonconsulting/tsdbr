context("package")

test_that("package", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
  file <- "ts.db"
  if(file.exists(file)) unlink(file)
  setup(ts_create_db(file = file, utc_offset = 8L))
  expect_true(file.exists(file))
  teardown(unlink(file))
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  teardown(DBI::dbDisconnect(conn))
  
  expect_is(conn, "SQLiteConnection")
  
  expect_error(DBI::dbGetQuery(conn, paste0("INSERT INTO Database VALUES(0);")), "only one row permitted!")

  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)

  expect_identical(parameters, ts_add_parameter("Temp", "degC", file = file))

  expect_is(ts_add_station("S1", "Temp", "day", file = file), "data.frame")

  stations <- data.frame(Station = "S2",
                         Parameter = "Temp",
                         Period = "hour",
                         LowerLimit = 0,
                         UpperLimit = 100,
                         stringsAsFactors = FALSE)

  expect_is(ts_add_stations(stations, file = file), "data.frame")

  data <- data.frame(Station = "S2", DateTime = ISOdate(2000, 9, 1, 0:23),
                     Recorded = 0:23 - 2,
                     stringsAsFactors = FALSE)
  
  data$Recorded[4] <- NA

  expect_error(ts_add_data(data, file = file), 
               "data[$]DateTime time zone must be 'Etc/GMT[+]8' [(]not 'GMT'[)]")
  data$DateTime <- ISOdate(2000, 9, 1, 0:23, tz = "Etc/GMT+8")
  
  expect_is(ts_add_data(data, file = file), "data.frame")

  expect_error(ts_add_data(data, file = file), "UNIQUE constraint failed: Data.Station, Data.DateTimeData")
  
  data$Recorded <- data$Recorded - 1

  expect_is(ts_add_data(data, file = file, resolution = "replace"), "data.frame")
  
  data$Station <- "S1"
  expect_error(ts_add_data(data, file = file), "invalid uploaded periods")
  
  expect_is(ts_add_data(data, file = file, aggregate = TRUE), "data.frame")
  
  data <- ts_get_data(stations = "S1", file = file)
  expect_is(data, "data.frame")
  expect_identical(ts_get_parameters(file = file), 
                   data.frame(Parameter = "Temp", Units = "degC", stringsAsFactors = FALSE))
  
  expect_identical(nrow(ts_get_stations(file = file)), 2L)
  
  expect_identical(ts_get_stations(file = file, periods = c("hour"))$Station, "S2")
 
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), status = "erroneous", file = file)), 25L)
  expect_identical(nrow(ts_get_data(end_date = as.Date("2000-09-01"), file = file)), 22L)
  expect_identical(nrow(ts_get_data(stations = "S1", end_date = as.Date("2000-09-01"), file = file)), 1L)
  expect_identical(nrow(ts_get_data(file = file)), 0L)
})

context("create")

test_that("ts_create", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
  file <- "ts.db"
  if(file.exists(file)) unlink(file)
  setup(ts_create(file, utc_offset = 8L))
  expect_true(file.exists(file))
  teardown(unlink(file))
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  teardown(DBI::dbDisconnect(conn))
  
  expect_is(conn, "SQLiteConnection")
  
  expect_error(DBI::dbGetQuery(conn, paste0("INSERT INTO Database VALUES(0);")), "only one row permitted!")

  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)

  expect_identical(parameters, ts_add_parameter("Temp", "degC", file))

  expect_is(ts_add_station("S1", "Temp", "hour", file = file), "data.frame")

  stations <- data.frame(Station = "S2",
                         Parameter = "Temp",
                         Period = "hour",
                         LowerLimit = 0,
                         UpperLimit = 100,
                         stringsAsFactors = FALSE)

  expect_is(ts_add_stations(stations, file), "data.frame")

  data <- data.frame(Station = "S2", DateTime = ISOdate(2000, 9, 1, 0:23),
                     Recorded = 0:23 - 2,
                     stringsAsFactors = FALSE)

  expect_error(ts_add_data(data, file), 
               "data[$]DateTime time zone must be 'Etc/GMT[+]8' [(]not 'GMT'[)]")
  data$DateTime <- ISOdate(2000, 9, 1, 0:23, tz = "Etc/GMT+8")
  
  expect_is(ts_add_data(data, file), "data.frame")
})

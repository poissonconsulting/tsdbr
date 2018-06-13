context("create")

test_that("ts_create", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
 # file <- "ts.db"
  setup(ts_create(file))
  expect_true(file.exists(file))

  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)

  expect_identical(parameters, ts_add_parameter("Temp", "degC", file))

  expect_is(ts_add_station("S1", "Temp", file), "data.frame")

  stations <- data.frame(Station = "S2",
                         Parameter = "Temp",
                         LowerLimit = 0,
                         UpperLimit = 100,
                         Longitude = NA_real_,
                         Latitude = NA_real_,
                         Organization = NA_character_,
                         StationName = NA_character_,
                         stringsAsFactors = FALSE)

  expect_identical(ts_add_stations(stations, file), stations)

  data <- data.frame(Station = "S2", DateTime = ISOdate(2000, 9, 1, 0:23),
                     Recorded = 0:23 - 2,
                     stringsAsFactors = FALSE)

  expect_is(ts_add_data(data, file), "data.frame")

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  expect_is(conn, "SQLiteConnection")

  teardown(DBI::dbDisconnect(conn))
  teardown(unlink(file))
})

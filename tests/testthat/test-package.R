context("create")

test_that("hdb_create", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
#  file <- "db.sqlite"
  setup(hdb_create(file))
  expect_true(file.exists(file))

  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)

  expect_identical(parameters, hdb_add_parameter("Temp", "degC", file))

  expect_is(hdb_add_station("S1", "Temp", file), "data.frame")

  stations <- data.frame(Station = "S2",
                         Parameter = "Temp",
                         LowerLimit = 0,
                         UpperLimit = 100,
                         Longitude = NA_real_,
                         Latitude = NA_real_,
                         Organization = NA_character_,
                         StationName = NA_character_,
                         stringsAsFactors = FALSE)

  expect_identical(hdb_add_stations(stations, file), stations)

  data <- data.frame(Station = "S2", DateReading = as.Date("2001-01-01"),
                     HourReading = 0:23, Value = 0:23 - 2, Corrected = 0:23 - 2,
                     Status = "Reasonable", Comments = NA_character_,
                     stringsAsFactors = FALSE)

  expect_is(hdb_add_data(data, file), "data.frame")

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  expect_is(conn, "SQLiteConnection")

  teardown(DBI::dbDisconnect(conn))
  teardown(unlink(file))
})

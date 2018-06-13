context("create")

test_that("hdb_create", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
  file <- "db.sqlite"
  setup(hdb_create(file))
  expect_true(file.exists(file))

  parameters <- data.frame(Parameter = "Temp",
                           Units = "degC", stringsAsFactors = FALSE)

  expect_identical(parameters, hdb_add_parameter("Temp", "degC", file))

  expect_is(hdb_add_station("S1", "Temp", Sys.Date(), file), "data.frame")

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  expect_is(conn, "SQLiteConnection")

  teardown(DBI::dbDisconnect(conn))
  teardown(unlink(file))
})

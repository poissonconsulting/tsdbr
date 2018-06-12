context("create")

test_that("hdb_create", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
  setup(hdb_create(file))
  expect_true(file.exists(file))

  parameters <- data.frame(Parameter = "Temperature",
                           Units = "degC", stringsAsFactors = FALSE)

  expect_identical(parameters, hdb_add_parameters(parameters, file))

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  expect_is(conn, "SQLiteConnection")

  teardown(DBI::dbDisconnect(conn))
  teardown(unlink(file))
})

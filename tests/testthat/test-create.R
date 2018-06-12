context("create")

test_that("hdb_create", {
  file <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".sqlite")
  setup(hdb_create(file))
  expect_true(file.exists(file))
  teardown(unlink(file))
})

context("utils")

test_that("ts_utc_offset_to_tz", {
  expect_identical(ts_utc_offset_to_tz(12L), "Etc/GMT-12")
  expect_identical(ts_utc_offset_to_tz(-8L), "Etc/GMT+8")
  expect_identical(ts_utc_offset_to_tz(0L), "GMT")
})
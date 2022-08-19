test_that("missing-in-the-middle imputes minimum and maximum values", {
  options(parttime.assume_tz_offset = 0)
  mitm <- as.parttime("2020---05", format = parse_cdisc_datetime)

  imp_mitm <- impute_time_max(mitm)
  expect_equal(imp_mitm[, "month"], 12L)
  expect_equal(imp_mitm[, "day"], 5L)
  expect_equal(imp_mitm[, "hour"], 23L)
  expect_equal(imp_mitm[, "min"], 59L)
  expect_equal(imp_mitm[, "sec"], 59)

  imp_mitm <- impute_time_min(mitm)
  expect_equal(imp_mitm[, "month"], 1L)
  expect_equal(imp_mitm[, "day"], 5L)
  expect_equal(imp_mitm[, "hour"], 0L)
  expect_equal(imp_mitm[, "min"], 0L)
  expect_equal(imp_mitm[, "sec"], 0)
})

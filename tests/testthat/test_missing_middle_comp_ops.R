test_that(paste0(
  "missing-in-the-middle dates use imputed upper and lower bounds for ",
  "comparison logic."
), {
  options(parttime.assume_tz_offset = 0)

  mitm <- as.parttime("2020---01", format = parse_cdisc_datetime)
  expect_true(is.na(mitm[, "month"]))

  expect_true(mitm < as.parttime("2021-01-01"))
  expect_true(mitm > as.parttime("2019-12-31"))
  expect_true(is.na(mitm > as.parttime("2020-01-01")))
  expect_true(is.na(mitm < as.parttime("2020-12-31")))
  expect_true(possibly(mitm > as.parttime("2020-01-01")))
  expect_true(possibly(mitm < as.parttime("2020-12-31")))
  expect_false(definitely(mitm > as.parttime("2020-01-01")))
  expect_false(definitely(mitm < as.parttime("2020-12-31")))

  mitm <- as.parttime("2020----T03", format = parse_cdisc_datetime)
  expect_true(is.na(mitm[, "month"]))
  expect_true(is.na(mitm[, "day"]))

  expect_true(mitm < as.parttime("2021-01-01T04"))
  expect_true(mitm > as.parttime("2019-01-01T02:59:59"))
  expect_true(is.na(mitm > as.parttime("2020-01-01T03:01")))
  expect_true(is.na(mitm < as.parttime("2020-01-01T03:01")))
})

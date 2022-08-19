test_that("impute_time_min populates fields with minimum appropriate data", {
  expect_equal(impute_time_min(NA), as.parttime(NA))

  expect_equal(impute_time_min("2022")[, "year"], 2022L)

  expect_equal(impute_time_min("2022-02")[, "month"], 2L)
  expect_equal(impute_time_min("2022")[, "month"], 1L)

  expect_equal(impute_time_min("2022-02")[, "day"], 1L)
  expect_equal(impute_time_min("2022-02-15")[, "day"], 15L)

  expect_equal(impute_time_min("2022-02-15")[, "hour"], 0L)
  expect_equal(impute_time_min("2022-02-15 03")[, "hour"], 3L)

  expect_equal(impute_time_min("2022-02-15 03")[, "min"], 0L)
  expect_equal(impute_time_min("2022-02-15 03:04")[, "min"], 4L)

  expect_equal(impute_time_min("2022-02-15 03:04")[, "sec"], 0L)
  expect_equal(impute_time_min("2022-02-15 03:04:05")[, "sec"], 5L)
  expect_equal(impute_time_min("2022-02-15 03:04:05.678")[, "sec"], 5.678)
})

test_that("impute_time_* operates on vectors of partial_time", {
  expect_equal(unname(impute_time_min(c("2022", "2022-02-02"))[, "day"]), c(1, 2))
  expect_equal(unname(impute_time_min(c(NA, "2022-02-02"))[, "day"]), c(NA, 2))

  expect_equal(unname(impute_time_max(c("2022", "2022-02-02"))[, "day"]), c(31, 2))
  expect_equal(unname(impute_time_max(c("2022", NA))[, "day"]), c(31, NA))
})

test_that("impute_time_max populates fields with maximum appropriate data", {
  expect_equal(impute_time_max(NA), as.parttime(NA))

  expect_equal(impute_time_max("2022")[, "year"], 2022L)

  expect_equal(impute_time_max("2022-02")[, "month"], 2L)
  expect_equal(impute_time_max("2022")[, "month"], 12L)

  expect_equal(impute_time_max("2024-01")[, "day"], 31L)
  expect_equal(impute_time_max("2022-02-15")[, "day"], 15L)

  expect_equal(impute_time_max("2022-02-15")[, "hour"], 23L)
  expect_equal(impute_time_max("2022-02-15 03")[, "hour"], 3L)

  expect_equal(impute_time_max("2022-02-15 03")[, "min"], 59L)
  expect_equal(impute_time_max("2022-02-15 03:04")[, "min"], 4L)

  expect_equal(impute_time_max("2022-02-15 03:04")[, "sec"], 59.999)
  expect_equal(impute_time_max("2022-02-15 03:04:05")[, "sec"], 5L)
})

test_that("impute_time_max considers month length and leap year month lengths", {
  expect_equal(impute_time_max("2022-02")[, "day"], 28L)
  expect_equal(impute_time_max("2024-02")[, "day"], 29L)
  expect_equal(impute_time_max("2024-01")[, "day"], 31L)
  expect_equal(impute_time_max("2024-04")[, "day"], 30L)
})

test_that("impute_* with resolution only imputes desired fields", {
  expect_equal(impute_time_min("2022", res = "day")[, "month"], 1)
  expect_equal(impute_time_min("2022", res = "day")[, "day"], 1)
  expect_equal(impute_time_min("2022", res = "day")[, "hour"], NA_real_)

  expect_equal(impute_time_min("2022", res = "min")[, "hour"], 0)
  expect_equal(impute_time_min("2022", res = "min")[, "min"], 0)
  expect_equal(impute_time_min("2022", res = "min")[, "sec"], NA_real_)

  expect_equal(impute_time_max("2022", res = "day")[, "month"], 12)
  expect_equal(impute_time_max("2022", res = "day")[, "day"], 31)
  expect_equal(impute_time_max("2022", res = "day")[, "hour"], NA_real_)

  expect_equal(impute_time_max("2022", res = "min")[, "hour"], 23)
  expect_equal(impute_time_max("2022", res = "min")[, "min"], 59)
  expect_equal(impute_time_max("2022", res = "min")[, "sec"], NA_real_)
})

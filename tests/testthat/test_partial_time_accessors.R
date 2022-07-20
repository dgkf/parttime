test_that("partial_time accessor functions extract field data", {
  expect_silent(withr::with_options(list(parttime.assume_tz_offset = 0), {
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
  }))

  # year
  expect_equal(names(year(pttms)), as.character(iso8601_dates))
  expect_equal(unname(year(pttms)), c(NA, 2000 + 1:14))

  # month
  expect_equal(names(month(pttms)), as.character(iso8601_dates))
  expect_equal(unname(month(pttms)), c(NA, NA, 3, 4, NA, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3))

  # day
  expect_equal(names(day(pttms)), as.character(iso8601_dates))
  expect_equal(unname(day(pttms)), c(NA, NA, 4, 5, NA, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

  # mday  (month-day)
  expect_equal(names(mday(pttms)), as.character(iso8601_dates))
  expect_equal(unname(mday(pttms)), c(NA, NA, 4, 5, NA, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

  # hour
  expect_equal(names(hour(pttms)), as.character(iso8601_dates))
  expect_equal(unname(hour(pttms)), c(NA, NA, NA, NA, NA, NA, 9, 10, 11, 12, 13, 14, 15, 16, 17))

  # minute
  expect_equal(names(minute(pttms)), as.character(iso8601_dates))
  expect_equal(unname(minute(pttms)), c(NA, NA, NA, NA, NA, NA, NA, 11, 12, 13, 14, 15, 16, 17, 18))

  # second
  expect_equal(names(second(pttms)), as.character(iso8601_dates))
  expect_equal(unname(second(pttms)), c(NA, NA, NA, NA, NA, NA, NA, NA, 13.014, 14.000, 15.016, 16.017, 17.018, 18.019, 19.020))

  # tz
  expect_equal(names(tz(pttms)), as.character(iso8601_dates))
  expect_equal(unname(tz(pttms)), c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 420, 450, 480))

  # tz after interpretting without assumptive timezone
  expect_silent(withr::with_options(list(parttime.assume_tz_offset = NULL), {
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
  }))

  expect_equal(names(tz(pttms)), as.character(iso8601_dates))
  expect_equal(unname(tz(pttms)), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 420, 450, 480))

  # tz after interpretting with non-0 assumptive timezone
  expect_silent(withr::with_options(list(parttime.assume_tz_offset = 120), {
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
  }))

  expect_equal(names(tz(pttms)), as.character(iso8601_dates))
  expect_equal(unname(tz(pttms)), c(NA, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 0, 420, 450, 480))
})

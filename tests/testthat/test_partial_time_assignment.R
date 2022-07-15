test_that("partial_time assignment helpers mutate field data", {
  expect_silent(withr::with_options(list(parttime.assume_tz_offset = 0), {
    pttms <- as.parttime(iso8601_dates)
  }))

  # year
  expect_silent(year(pttms) <- 2000 - 1:15)
  expect_equal(unname(pttms[, "year"]), 2000 - 1:15)

  # month
  expect_silent(month(pttms) <- 0:14 %% 12 + 1)
  expect_equal(unname(pttms[, "month"]), 0:14 %% 12 + 1)

  # day
  expect_silent(day(pttms) <- 1:15 * 2)
  expect_equal(unname(pttms[, "day"]), 1:15 * 2)

  # hour
  expect_silent(hour(pttms) <- (1:15 * 3) %% 24)
  expect_equal(unname(pttms[, "hour"]), (1:15 * 3) %% 24)

  # minute
  expect_silent(minute(pttms) <- (1:15 * 4) %% 60)
  expect_equal(unname(pttms[, "min"]), (1:15 * 4) %% 60)

  # second
  expect_silent(second(pttms) <- (1:15 * 7) %% 60)
  expect_equal(unname(pttms[, "sec"]), (1:15 * 7) %% 60)

  # tz
  expect_silent(tz(pttms) <- 1:15 * 20)
  expect_equal(unname(pttms[, "tzhour"] * 60 + pttms[, "tzmin"]), 1:15 * 20)
})

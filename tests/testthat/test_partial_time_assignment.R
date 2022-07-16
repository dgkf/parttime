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

test_that("partial_time assignment with out-of-range values reflows fields", {
  pttm <- parttime(year = 2003, month = 4, day = 5)
  expect_equal(pttm[[1, "year"]], 2003)
  expect_equal(pttm[[1, "month"]], 4)
  expect_silent(pttm[[1, "month"]] <- 13)
  expect_equal(pttm[[1, "year"]], 2004)

  # expect that reflowing for leap days rolls into leap day
  pttm <- parttime(year = 2004, month = 2, day = 28, hour = 23)
  expect_true(lubridate::leap_year(pttm[[1, "year"]]))
  expect_silent(pttm[[1, "hour"]] <- 25)
  expect_equal(pttm[[1, "month"]], 2)
  expect_equal(pttm[[1, "day"]], 29)

  # expect that reflowing for non-leap days does not roll into a leap day
  pttm <- parttime(year = 2003, month = 2, day = 28, hour = 23)
  expect_false(lubridate::leap_year(pttm[[1, "year"]]))
  expect_silent(pttm[[1, "hour"]] <- 25)
  expect_equal(pttm[[1, "month"]], 3)
  expect_equal(pttm[[1, "day"]], 1)
})

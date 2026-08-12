withr::local_options(parttime.assume_tz_offset = 0, .local_envir = teardown_env())

test_that("to_gmt() subtracts the offset", {
  # UTC = local - offset, so a zone ahead of UTC reads earlier and one behind
  # reads later, and the date follows when that crosses midnight.
  cases <- c(
    "2015-04-13T23:30:00+02:00" = "2015-04-13 21:30:00",
    "2015-04-13T02:30:00-05:00" = "2015-04-13 07:30:00",
    "2015-04-13T02:30:00-05:45" = "2015-04-13 08:15:00",  # Nepal, off the hour
    "2015-04-13T01:00:00+02:00" = "2015-04-12 23:00:00",
    "2015-04-13T23:00:00-02:00" = "2015-04-14 01:00:00"
  )
  expect_equal(
    crayon::strip_style(format(to_gmt(as.parttime(names(cases))), quote = FALSE)),
    unname(cases)
  )
})

test_that("to_gmt() agrees with as.POSIXct()", {
  x <- as.parttime("2015-04-13T23:30:00+02:00")
  expect_equal(as.POSIXct(to_gmt(x)), as.POSIXct(x))
})

test_that("a comparison across two recorded offsets is resolved", {
  a <- as.parttime("2015-04-13T10:30:00+02:00")  # 08:30 UTC
  b <- as.parttime("2015-04-13T12:00:00+00:00")  # 12:00 UTC
  expect_equal(c(definitely(a < b), definitely(a > b)), c(TRUE, FALSE))
})

test_that("an unknown offset widens the window by its full range", {
  # An offset spans -12:00 to +14:00, more than a day, so February can precede
  # January once the zone is unknown; two months apart is beyond any offset.
  withr::local_options(parttime.assume_tz_offset = NA)
  # Both sides are length two because comparison does not recycle.
  expect_equal(
    possibly(parttime(2019, c(2, 4)) < parttime(2019, c(1, 1))),
    c(TRUE, FALSE)
  )
})

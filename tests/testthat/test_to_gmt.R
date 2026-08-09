fmt <- function(x) crayon::strip_style(format(x))

test_that("to_gmt() subtracts the offset", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    # 23:30 in a zone two hours ahead of UTC is 21:30 UTC, on the same day.
    x <- as.parttime("2015-04-13T23:30:00+02:00")
    expect_equal(fmt(to_gmt(x)), "\"2015-04-13 21:30:00\"")
    expect_equal(
      format(as.POSIXct(x), tz = "UTC"),
      format(as.POSIXct(to_gmt(x)), tz = "UTC")
    )
  })
})

test_that("to_gmt() handles a negative and a fractional offset", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    # Behind UTC, so the UTC instant is later.
    expect_equal(
      fmt(to_gmt(as.parttime("2015-04-13T02:30:00-05:00"))),
      "\"2015-04-13 07:30:00\""
    )
    # Nepal is 5 hours 45 minutes ahead.
    expect_equal(
      fmt(to_gmt(as.parttime("2015-04-13T02:30:00-05:45"))),
      "\"2015-04-13 08:15:00\""
    )
    expect_equal(
      fmt(to_gmt(as.parttime("2015-04-13T10:30:00+05:45"))),
      "\"2015-04-13 04:45:00\""
    )
  })
})

test_that("to_gmt() rolls the date when the offset crosses midnight", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_equal(
      fmt(to_gmt(as.parttime("2015-04-13T01:00:00+02:00"))),
      "\"2015-04-12 23:00:00\""
    )
    expect_equal(
      fmt(to_gmt(as.parttime("2015-04-13T23:00:00-02:00"))),
      "\"2015-04-14 01:00:00\""
    )
  })
})

test_that("a comparison across two recorded offsets is resolved correctly", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    a <- as.parttime("2015-04-13T10:30:00+02:00")  # 08:30 UTC
    b <- as.parttime("2015-04-13T12:00:00+00:00")  # 12:00 UTC
    expect_true(definitely(a < b))
    expect_false(definitely(a > b))
    expect_true(possibly(a < b))
  })
})

test_that("an unknown offset still widens the window both ways", {
  withr::with_options(list(parttime.assume_tz_offset = NA), {
    # An offset can span -12:00 to +14:00, more than a day, so a February
    # value can precede a January one once the zone is unknown.
    expect_true(possibly(parttime(2019, 2) < parttime(2019, 1)))
    # Two months apart is beyond any offset, so it cannot.
    expect_false(possibly(parttime(2019, 4) < parttime(2019, 1)))
  })
})

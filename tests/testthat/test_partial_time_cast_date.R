withr::local_options(parttime.assume_tz_offset = 0, .local_envir = teardown_env())

test_that("a Date casts to the same value its date string does", {
  # The components a Date carries are known; the time of day is missing rather
  # than zero, and a calendar date names no offset so it takes the assumed one.
  # Row names carry the text a value was parsed from, which a Date has none of.
  expect_equal(
    unname(vctrs::field(as.parttime(as.Date("2001-01-01")), "pttm_mat")),
    unname(vctrs::field(as.parttime("2001-01-01"), "pttm_mat"))
  )
})

test_that("a date-time casts to the same value its timestamp does", {
  expect_equal(
    unname(vctrs::field(as.parttime(as.POSIXct("2001-06-15 10:30:15", tz = "UTC")), "pttm_mat")),
    unname(vctrs::field(as.parttime("2001-06-15T10:30:15+00:00"), "pttm_mat"))
  )
})

test_that("the offset is read from the value, not assumed", {
  # The offset in force at that instant, so a zone observing daylight saving
  # gives different answers either side of the change, and a zone off the hour
  # keeps its minutes.  Cast one at a time: `c()` on `POSIXct` re-zones every
  # element to the first one's timezone.
  expect_equal(
    unname(c(
      vctrs::field(as.parttime(as.POSIXct("2001-01-15 10:30:15", tz = "America/New_York")), "pttm_mat")[1, "tzhour"],
      vctrs::field(as.parttime(as.POSIXct("2001-06-15 10:30:15", tz = "America/New_York")), "pttm_mat")[1, "tzhour"],
      vctrs::field(as.parttime(as.POSIXct("2001-06-15 10:30:15", tz = "Asia/Kathmandu")), "pttm_mat")[1, "tzhour"]
    )),
    c(-5, -4, 5.75)
  )
})

test_that("every component of a date-time survives the cast", {
  x <- as.POSIXct("2001-06-15 10:30:15.25", tz = "UTC")
  expect_equal(
    unname(vctrs::field(as.parttime(x), "pttm_mat")[1, ]),
    c(2001, 6, 15, 10, 30, 15.25, 0)
  )
})

test_that("a POSIXlt casts as a POSIXct does", {
  expect_equal(
    vctrs::field(as.parttime(as.POSIXlt("2001-06-15 10:30:15", tz = "UTC")), "pttm_mat"),
    vctrs::field(as.parttime(as.POSIXct("2001-06-15 10:30:15", tz = "UTC")), "pttm_mat")
  )
})

test_that("a missing date is missing throughout", {
  # Taking the assumed offset for a date nobody recorded would leave a value
  # with no date, no time and an offset, which reads as present.
  expect_true(all(is.na(vctrs::field(as.parttime(as.Date(NA)), "pttm_mat"))))
  expect_true(all(is.na(vctrs::field(as.parttime(as.POSIXct(NA)), "pttm_mat"))))
  expect_equal(
    is.na(as.parttime(as.Date(c("2001-01-01", NA, "2002-02-02")))),
    c(FALSE, TRUE, FALSE)
  )
})

test_that("a Date component that was never collected can be marked per element", {
  # A month-precision value read into a Date lands on the first of the month, so
  # only that element's day is an artefact.  The rest keep the day they carry.
  expect_equal(
    unname(vctrs::field(
      as.parttime(
        as.Date(c("2001-01-15", "2001-02-01", "2001-03-20")),
        missing_day = c(FALSE, TRUE, FALSE)
      ),
      "pttm_mat"
    )[, "day"]),
    c(15, NA, 20)
  )
})

test_that("a mask recycles to the length of the input", {
  expect_equal(
    unname(vctrs::field(
      as.parttime(as.Date(c("2001-01-15", "2001-02-20")), missing_month = TRUE, missing_day = TRUE),
      "pttm_mat"
    )[, c("year", "month", "day")]),
    cbind(c(2001, 2001), c(NA, NA), c(NA, NA))
  )
})

test_that("a date-time can be marked missing at every level of precision", {
  x <- as.POSIXct("2001-06-15 10:30:15", tz = "UTC")
  expect_equal(
    unname(vctrs::field(
      as.parttime(x, missing_second = TRUE),
      "pttm_mat"
    )[1, ]),
    c(2001, 6, 15, 10, 30, NA, 0)
  )
  expect_equal(
    unname(vctrs::field(
      as.parttime(x, missing_hour = TRUE, missing_minute = TRUE, missing_second = TRUE),
      "pttm_mat"
    )[1, ]),
    c(2001, 6, 15, NA, NA, NA, 0)
  )
  expect_equal(
    unname(vctrs::field(
      as.parttime(
        x,
        missing_year = TRUE, missing_month = TRUE, missing_day = TRUE,
        missing_hour = TRUE, missing_minute = TRUE, missing_second = TRUE,
        missing_tz = TRUE
      ),
      "pttm_mat"
    )[1, ]),
    rep(NA_real_, 7)
  )
})

test_that("the offset can be marked missing on its own", {
  # A zoned value whose offset is an artefact of the reader, not collected.
  expect_equal(
    unname(vctrs::field(
      as.parttime(
        as.POSIXct("2001-06-15 10:30:15", tz = "America/New_York"),
        missing_tz = TRUE
      ),
      "pttm_mat"
    )[1, ]),
    c(2001, 6, 15, 10, 30, 15, NA)
  )
})

test_that("a mask marks elements and cannot fabricate one", {
  # Marking only ever removes what the input carried, so a value that was
  # missing to begin with stays missing.
  expect_equal(
    is.na(as.parttime(as.Date(c("2001-01-01", NA)), missing_day = TRUE)),
    c(FALSE, TRUE)
  )
  expect_true(all(is.na(vctrs::field(
    as.parttime(as.POSIXct(NA), missing_second = TRUE), "pttm_mat"
  ))))
})

test_that("a mask must be logical, the right length, and not itself NA", {
  d <- as.Date(c("2001-01-15", "2001-02-20"))
  # Every message names the argument it came from, rather than an internal one.
  expect_error(as.parttime(d, missing_day = 2), class = "vctrs_error_cast_lossy")
  expect_error(as.parttime(d, missing_day = "x"), class = "vctrs_error_cast")
  expect_error(
    as.parttime(d, missing_day = c(TRUE, FALSE, TRUE)),
    class = "vctrs_error_incompatible_size"
  )
  expect_error(as.parttime(d, missing_day = NA), "cannot .*be .*NA")
  expect_match(
    conditionMessage(expect_error(as.parttime(d, missing_day = "x"))),
    "missing_day"
  )
})

test_that("degenerate input", {
  expect_equal(length(as.parttime(as.Date(character(0)))), 0L)
  expect_equal(length(as.parttime(as.POSIXct(character(0)))), 0L)
  expect_equal(length(as.parttime(as.Date(c("2001-01-01", "2002-02-02")))), 2L)
})

test_that("a type that cannot convert says which", {
  # Both type names appear, so a caller learns what would not convert.
  cnd <- expect_error(as.parttime(list(1, 2)), class = "vctrs_error_cast")
  expect_match(conditionMessage(cnd), "<list>")
  expect_match(conditionMessage(cnd), "<partial_time>")
})

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

test_that("the time a Date does not carry can be given a value", {
  # What a caller wants when a Date stands in for a datetime: the midnight that
  # starts the date.  Set singly, the components not named stay unknown.
  expect_equal(
    unname(vctrs::field(
      as.parttime(
        as.Date("2001-01-01"),
        missing_hour = 0, missing_minute = 0, missing_second = 0
      ),
      "pttm_mat"
    )[1, ]),
    c(2001, 1, 1, 0, 0, 0, 0)
  )
  expect_equal(
    unname(vctrs::field(
      as.parttime(as.Date("2001-01-01"), missing_hour = 12),
      "pttm_mat"
    )[1, ]),
    c(2001, 1, 1, 12, NA, NA, 0)
  )
})

test_that("a POSIXt component that is missing can be given a value", {
  # A `POSIXlt` can be built with a component missing where the date is known.
  lt <- as.POSIXlt("2001-06-15 10:30:15", tz = "UTC")
  lt$sec <- NA_real_
  expect_equal(
    unname(vctrs::field(as.parttime(lt, missing_second = 0), "pttm_mat")[1, ]),
    c(2001, 6, 15, 10, 30, 0, NA)
  )
})

test_that("a component that was recorded is not overwritten", {
  # A `POSIXct` carries every component, so the arguments have nothing to fill
  # and cannot replace the time that was actually recorded.
  expect_equal(
    unname(vctrs::field(
      as.parttime(
        as.POSIXct("2001-06-15 10:30:15", tz = "UTC"),
        missing_hour = 9, missing_minute = 9, missing_second = 0
      ),
      "pttm_mat"
    )[1, ]),
    c(2001, 6, 15, 10, 30, 15, 0)
  )
})

test_that("a missing value stays missing whatever the time is set to", {
  # Filling the time of a date nobody recorded would leave a value with no date
  # but a time, which reads as present.
  expect_equal(
    is.na(as.parttime(
      as.Date(c("2001-01-01", NA)),
      missing_hour = 0, missing_minute = 0, missing_second = 0
    )),
    c(FALSE, TRUE)
  )
  expect_equal(
    c(
      all(is.na(vctrs::field(
        as.parttime(as.Date(NA), missing_hour = 0, missing_minute = 0, missing_second = 0),
        "pttm_mat"
      ))),
      all(is.na(vctrs::field(
        as.parttime(as.POSIXct(NA), missing_hour = 0, missing_minute = 0, missing_second = 0),
        "pttm_mat"
      )))
    ),
    c(TRUE, TRUE)
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

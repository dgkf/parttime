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

test_that("degenerate input", {
  expect_equal(length(as.parttime(as.Date(character(0)))), 0L)
  expect_equal(length(as.parttime(as.POSIXct(character(0)))), 0L)
  expect_equal(length(as.parttime(as.Date(c("2001-01-01", "2002-02-02")))), 2L)
})

test_that("a type that cannot convert says which", {
  # The refusal used to be an error about a missing formal of
  # `stop_incompatible_cast()`, which named neither type.
  cnd <- expect_error(as.parttime(list(1, 2)), class = "vctrs_error_cast")
  expect_match(conditionMessage(cnd), "<list>")
  expect_match(conditionMessage(cnd), "<partial_time>")
})

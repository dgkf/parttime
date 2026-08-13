test_that("an offset is read from the '+HHMM' form, in minutes", {
  expect_equal(
    interpret_tz(c("+0530", "-0400", "+0000", "-0545", "0530", "530", "-0000")),
    c(330, -240, 0, -345, 330, 330, 0)
  )
})

test_that("a timezone name is resolved, and may be mixed with offsets", {
  expect_equal(
    interpret_tz(c("UTC", "GMT", "America/New_York", "+0530")),
    c(0, 0, -300, 330)
  )
})

test_that("a numeric offset passes through unchanged", {
  # Numeric input is already minutes, so there is nothing to interpret.
  expect_equal(interpret_tz(c(0, 5.5, -4)), c(0, 5.5, -4))
  expect_equal(interpret_tz(NA), NA)
})

test_that("a missing timezone is a missing offset", {
  # `parttime.assume_tz_offset` is unset far more often than not, and a value
  # whose offset was never recorded has to survive as far as the parttime.
  expect_equal(
    interpret_tz(c("+0530", NA, "UTC", NA_character_)),
    c(330, NA, 0, NA)
  )
})

test_that("an unrecognised timezone is an error, not a missing offset", {
  # A mistyped zone read as an unknown offset would be a silently wrong value.
  # The colon form is not accepted either, and says so the same way.
  expect_error(interpret_tz("bogus"), "Invalid timezone")
  expect_error(interpret_tz(""), "Invalid timezone")
  expect_error(interpret_tz("+05:30"), "Invalid timezone")
  expect_error(interpret_tz(c("+0530", "bogus")), "Invalid timezone")
})

test_that("degenerate input", {
  expect_equal(interpret_tz(character(0)), numeric(0))
  expect_equal(interpret_tz(numeric(0)), numeric(0))
  expect_equal(interpret_tz(NULL), NULL)
})

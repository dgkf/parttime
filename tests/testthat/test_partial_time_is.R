test_that("the UTC offset does not count towards a value being present", {
  # `parttime.assume_tz_offset` fills the offset when the value carried none,
  # so counting it reported an empty value as present whatever the option said.
  for (opt in list(0, NA)) {
    withr::local_options(parttime.assume_tz_offset = opt)
    expect_equal(
      is.na(as.parttime(c(NA, "2015", "2015-04-13", "2015-04-13T10:30:15"))),
      c(TRUE, FALSE, FALSE, FALSE)
    )
    # A logical `NA` is cast by another path, and an offset with no date or
    # time behind it is the case the fix is named for.
    expect_true(is.na(as.parttime(NA)))
    expect_true(is.na(parttime(tzhour = 2)))
  }
})

test_that("is.na() handles degenerate input", {
  withr::local_options(parttime.assume_tz_offset = 0)
  expect_equal(is.na(as.parttime(character(0))), logical(0))
  expect_equal(is.na(as.parttime("2015")), FALSE)
})

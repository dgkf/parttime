test_that("a value with nothing recorded is NA whatever the offset option", {
  # The offset is filled from the option when the value carried none, so
  # counting it would report an empty value as present.
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_true(is.na(as.parttime(NA)))
    expect_true(is.na(as.parttime(NA_character_)))
  })
  withr::with_options(list(parttime.assume_tz_offset = NA), {
    expect_true(is.na(as.parttime(NA)))
    expect_true(is.na(as.parttime(NA_character_)))
  })
})

test_that("a value with any component recorded is not NA", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_false(is.na(as.parttime("2015")))
    expect_false(is.na(as.parttime("2015-04-13")))
    expect_false(is.na(as.parttime("2015-04-13T10:30:15")))
  })
})

test_that("an offset on its own does not make a value present", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    only_tz <- parttime(tzhour = 2)
    expect_true(is.na(only_tz))
  })
})

test_that("is.na() is elementwise and handles degenerate input", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_equal(is.na(as.parttime(c("2015", NA, "2016-02"))), c(FALSE, TRUE, FALSE))
    expect_equal(is.na(as.parttime(character(0))), logical(0))
    expect_equal(is.na(as.parttime("2015")), FALSE)
  })
})

fmt <- function(x) crayon::strip_style(format(x, quote = FALSE))

pttm_from <- function(dtc) {
  m <- suppressWarnings(parse_cdisc_datetime(dtc))
  parttime(
    year = m[, "year"], month = m[, "month"], day = m[, "day"],
    hour = m[, "hour"], min = m[, "min"], sec = m[, "sec"]
  )
}

test_that("an unknown component does not print as a zero", {
  # Styling alone cannot carry this: `style_na()` is colour, so a month nobody
  # recorded used to be indistinguishable from January once styling was
  # stripped or the value was written to a file.
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_false(grepl("00", fmt(pttm_from("2015---13"))))
    expect_match(fmt(pttm_from("2015---13")), "^2015-+13$")
    expect_match(fmt(pttm_from("2015-04-13T-:30")), "--:30$")
  })
})

test_that("an unknown year is not printed as year zero", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_false(grepl("0000", fmt(pttm_from("--04-13"))))
  })
})

test_that("recorded zeros still print as zeros", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    # Midnight is a time somebody recorded, not an absence.
    expect_equal(fmt(as.parttime("2015-04-13T00:00:00")), "2015-04-13 00:00:00")
    expect_equal(fmt(as.parttime("2015-01-01")), "2015-01-01")
  })
})

test_that("a value that simply stops short is unchanged", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_equal(fmt(as.parttime("2015")), "2015")
    expect_equal(fmt(as.parttime("2015-04")), "2015-04")
    expect_equal(fmt(as.parttime("2015-04-13")), "2015-04-13")
    expect_equal(fmt(as.parttime("2015-04-13T10:30")), "2015-04-13 10:30")
  })
})

test_that("a missing value still prints as NA", {
  withr::with_options(list(parttime.assume_tz_offset = 0), {
    expect_equal(fmt(as.parttime(NA_character_)), "NA")
  })
})

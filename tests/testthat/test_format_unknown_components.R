withr::local_options(parttime.assume_tz_offset = 0, .local_envir = teardown_env())

test_that("an unknown component is not printed as a zero", {
  # `style_na()` is colour, so a component nobody recorded used to be
  # indistinguishable from a collected one wherever styling was stripped.
  m <- suppressWarnings(
    parse_cdisc_datetime(c("2015---13", "--04-13", "2015-04-13T-:30"))
  )
  x <- parttime(
    year = m[, "year"], month = m[, "month"], day = m[, "day"],
    hour = m[, "hour"], min = m[, "min"], sec = m[, "sec"]
  )
  expect_equal(
    crayon::strip_style(format(x, quote = FALSE)),
    c("2015-??-13", "????-04-13", "2015-04-13 ??:30")
  )
})

test_that("collected values are printed as they were", {
  # The other half: a midnight and a January are values, not absences, and a
  # value that stops short keeps stopping short.
  same <- c("2015-04-13 00:00:00", "2015-01-01", "2015", "2015-04",
            "2015-04-13", "2015-04-13 10:30")
  expect_equal(
    crayon::strip_style(format(as.parttime(sub(" ", "T", same)), quote = FALSE)),
    same
  )
  expect_equal(
    crayon::strip_style(format(as.parttime(NA_character_), quote = FALSE)),
    "NA"
  )
})

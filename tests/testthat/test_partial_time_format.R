test_that("parttime formats to iso8601-style as character", {
  expect_silent(withr::with_options(list(parttime.assume_tz_offset = 0), {
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
  }))

  lapply(pttms, function(pttm) {
    fmt       <- crayon::strip_style(format(pttm))
    fmt_tz    <- crayon::strip_style(format(pttm, tz = TRUE))
    fmt_no_tz <- crayon::strip_style(format(pttm, tz = FALSE))

    if (is.na(pttm))
      expect_equal(fmt, "NA")

    if (!is.na(pttm[, "year"]))
      expect_match(fmt, sprintf("%4.f", pttm[, "year"]))

    if (!is.na(pttm[, "month"]))
      expect_match(fmt, sprintf("%02.f", pttm[, "month"]))

    if (!is.na(pttm[, "day"]))
      expect_match(fmt, sprintf("%02.f", pttm[, "day"]))

    if (!is.na(pttm[, "hour"]))
      expect_match(fmt, sprintf("%02.f", pttm[, "hour"]))

    if (!is.na(pttm[, "min"]))
      expect_match(fmt, sprintf("%02.f", pttm[, "min"]))

    if (!is.na(pttm[, "sec"]))
      expect_match(fmt, sprintf(if (pttm[,"sec"] %% 1 == 0) "%02.f" else "%02.3f", pttm[, "sec"]))

    if (is.na(pttm[, "tzhour"]))
      expect_no_match(fmt, "[+-]\\d{2}:\\d{2}$")

    if (!is.na(pttm[, "tzhour"]))
      expect_match(fmt_tz, sprintf("%02.f:%02.f", pttm[, "tzhour"] %/% 1, pttm[, "tzhour"] %% 1 * 60))

    if (!is.na(pttm[, "tzhour"]))
      expect_no_match(fmt_no_tz, sprintf("%04.f$", pttm[, "tzhour"] * 60))
  })
})

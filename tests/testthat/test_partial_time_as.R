test_that("as.character can convert partial_time to represenative string format", {
  expect_silent({
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
    pttms_chars <- as.character(pttms)
  })

  for (i in seq_along(iso8601_dates)) {
    if (is.na(iso8601_dates[[i]])) {
      expect_equal(pttms_chars[[i]], NA_character_)
      next
    }

    expect_true(startsWith(pttms_chars[[i]], substring(iso8601_dates[[i]], 1, 4)))

    if (!is.na(pttms[[i, "month"]]))
      expect_match(pttms_chars[[i]], sprintf("\\b-%02.f\\b", pttms[[i, "month"]]))

    if (!is.na(pttms[[i, "day"]]))
      expect_match(pttms_chars[[i]], sprintf("\\b-%02.f\\b", pttms[[i, "day"]]))

    if (!is.na(pttms[[i, "hour"]]))
      expect_match(pttms_chars[[i]], sprintf("\\b %02.f\\b", pttms[[i, "hour"]]))

    if (!is.na(pttms[[i, "min"]]))
      expect_match(pttms_chars[[i]], sprintf("\\b:%02.f\\b", pttms[[i, "min"]]))

    if (!is.na(pttms[[i, "sec"]]))
      expect_match(pttms_chars[[i]], sprintf("\\b:%02.f\\b", pttms[[i, "sec"]]))

    if (!is.na(pttms[[i, "secfrac"]]) && pttms[[i, "secfrac"]] != 0)
      expect_match(pttms_chars[[i]], substring(sprintf("%.03f\\b", pttms[[i, "secfrac"]]), 2L))
  }
})

test_that("as.parttime.character emits warnings when empty strings are converted to NA", {
  expect_warning(as.parttime(""), "could not be parsed")
  expect_warning(as.parttime("  "), "could not be parsed")
})

test_that("as.parttime.character emits warnings provides a sampling of unparsable formats", {
  expect_warning(
    as.parttime(c("2022", "2023", "Y13", "Y15", "1999-01")),
    "could not be parsed"
  )

  expect_warning(
    as.parttime(c("2022", "2023", "Y13", "Y15", "1999-01")),
    "Y13"  # first of Y## format is displayed as example
  )

  expect_warning(
    as.parttime(c("2022", "2023", "Y13", "Y15", "W3", "W5", "1999-01")),
    "Y13.*W3"  # first of each invalid format is displayed, Y## and W#
  )
})

test_that("as.parttime.character on.na respects character input options ", {
  expect_error(as.parttime(c("2022", "X13"), on.na = "error"))
  expect_warning(as.parttime(c("2022", "X13"), on.na = "warning"))
  expect_silent(as.parttime(c("2022", "X13"), on.na = "suppress"))
})

test_that("as.parttime.character on.na respects signaling function input options ", {
  expect_error(as.parttime(c("2022", "X13"), on.na = stop))
  expect_warning(as.parttime(c("2022", "X13"), on.na = warning))
})

test_that("as.parttime.character on.na respects NULL input options ", {
  expect_silent(as.parttime(c("2022", "X13"), on.na = NULL))
})

test_that("as.POSIXct can convert partial_time to representative posix time", {
  expect_silent({
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
    pttms_ct <- as.POSIXct(pttms)
  })

  # expect that any date defined down to the day provides a valid POSIXct value
  expect_true(all(is.na(pttms_ct[is.na(pttms[, "day"])])))

  # expect that any date without day produces NA POSIXct value
  expect_true(all(!is.na(pttms_ct[!is.na(pttms[, "day"])])))
})

test_that("as.POSIXct can convert partial_time to representative posix time", {
  expect_silent({
    pttms <- as.parttime(iso8601_dates, warn = FALSE)
    pttms_ct <- as.POSIXlt(pttms)
  })

  # expect that any date defined down to the day provides a valid POSIXlt value
  expect_true(all(is.na(pttms_ct[is.na(pttms[, "day"])])))

  # expect that any date without day produces NA POSIXlt value
  expect_true(all(!is.na(pttms_ct[!is.na(pttms[, "day"])])))
})

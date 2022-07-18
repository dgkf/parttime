test_that("as.character can convert partial_time to represenative string format", {
  expect_silent({
    pttms <- as.parttime(iso8601_dates)
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

test_that("as.POSIXct can convert partial_time to representative posix time", {
  expect_silent({
    pttms <- as.parttime(iso8601_dates)
    pttms_ct <- as.POSIXct(pttms)
  })

  # expect that any date defined down to the day provides a valid POSIXct value
  expect_true(all(is.na(pttms_ct[is.na(pttms[, "day"])])))

  # expect that any date without day produces NA POSIXct value
  expect_true(all(!is.na(pttms_ct[!is.na(pttms[, "day"])])))
})

test_that("as.POSIXct can convert partial_time to representative posix time", {
  expect_silent({
    pttms <- as.parttime(iso8601_dates)
    pttms_ct <- as.POSIXlt(pttms)
  })

  # expect that any date defined down to the day provides a valid POSIXlt value
  expect_true(all(is.na(pttms_ct[is.na(pttms[, "day"])])))

  # expect that any date without day produces NA POSIXlt value
  expect_true(all(!is.na(pttms_ct[!is.na(pttms[, "day"])])))
})

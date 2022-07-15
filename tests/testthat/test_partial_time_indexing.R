test_that("parttime indexing works with base indexing operators", {
  expect_silent(withr::with_options(list(parttime.assume_tz_offset = 0), {
    pttms_1 <- as.parttime(iso8601_dates[[1]])
    pttms_1_3 <- as.parttime(iso8601_dates[1:3])
    pttms <- as.parttime(iso8601_dates)
  }))

  # rownames are kept when indexing
  expect_equal(names(pttms[, "year"]), iso8601_dates)

  # indexing by field
  expect_equal(unname(pttms[, "year"]), c(NA, 2000 + 1:14))

  # indexing with multiple fields
  expect_equal(
    unname(pttms[, c("year", "month")]),
    cbind(c(NA, 2000 + 1:14), c(NA, NA, 3, 4, NA, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3))
  )

  expect_equal(colnames(pttms[, c("year", "month")]), c("year", "month"))
  expect_equal(rownames(pttms[, c("year", "month")]), iso8601_dates)

  # indexing rows with multiple fields
  expect_equal(
    unname(pttms[1:2, c("year", "month")]),
    cbind(c(NA, 2001), c(NA, NA))
  )

  # indexing individual values
  expect_equal(pttms[[3L, "year"]], 2002)

  # indexing without field names subsets as vector
  expect_equal(pttms[1:3], pttms_1_3)

  # indexing individual value without field
  expect_equal(pttms[[1]], pttms_1)
})

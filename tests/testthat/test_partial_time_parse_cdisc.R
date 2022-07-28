test_that("parsing cdisc dates works for example date strings", {
  pttms <- expect_silent(as.parttime(cdisc_datetimes_decreasing_sensitivity, format = parse_cdisc_datetime))
  pttm_mat <- expect_silent(as.matrix(pttms))

  expect_true(all(pttm_mat[, "year"] == 2003))

  expect_true(all(pttm_mat[1:6, "month"] == 12))
  expect_true(all(is.na(pttm_mat[7, "month"])))

  expect_true(all(pttm_mat[1:5, "day"] == 15))
  expect_true(all(is.na(pttm_mat[6:7, "day"])))

  expect_true(all(pttm_mat[1:4, "hour"] == 13))
  expect_true(all(is.na(pttm_mat[5:7, "hour"])))

  expect_true(all(pttm_mat[1:3, "min"] == 14))
  expect_true(all(is.na(pttm_mat[4:7, "min"])))

  expect_true(all(pttm_mat[1:2, "sec"] == 17))
  expect_true(all(is.na(pttm_mat[3:7, "sec"])))

  expect_true(all(pttm_mat[1:2, "secfrac"] == c(0.123, 0)))
  expect_true(all(is.na(pttm_mat[3:7, "secfrac"])))
})

test_that("parsing cdisc dates requires dash separator", {
  no_seps <- c("20220101", "20220101T01:02", "20220101T0102", "2022-01-01T0102")
  pttms <- expect_warning(as.parttime(no_seps, format = parse_cdisc_datetime))
  expect_true(all(is.na(pttms)))
})

test_that("parsing cdisc dates disallows comma fraction separators", {
  comma_seps <- c("2022-01-01T01:01:01,0")
  pttms <- expect_warning(as.parttime(comma_seps, format = parse_cdisc_datetime))
  expect_true(all(is.na(pttms)))
})

test_that("parsing cdisc missing-in-the-middle throws warning for experimental feature", {
  pttms <- expect_warning(
    as.parttime(cdisc_missing_in_middle, format = parse_cdisc_datetime),
    "experimental"
  )
})

test_that("parsing cdisc missing-in-the-middle preserves proper NAs", {
  pttms <- as.parttime(cdisc_missing_in_middle, format = parse_cdisc_datetime)
  pttm_mat <- expect_silent(as.matrix(pttms))

  expect_true(all(pttm_mat[c(1:4, 8:12), "year"] == 2003))
  expect_true(all(is.na(pttm_mat[-c(1:4, 8:12), "year"])))

  expect_true(all(pttm_mat[c(1:3, 5, 7, 9:12, 14:17), "month"] == 12))
  expect_true(all(is.na(pttm_mat[-c(1:3, 5, 7, 9:12, 14:17), "month"])))

  expect_true(all(pttm_mat[c(1:5, 7:8, 10:13, 15:17), "day"] == 15))
  expect_true(all(is.na(pttm_mat[-c(1:5, 7:8, 10:13, 15:17), "day"])))

  expect_true(all(pttm_mat[c(1, 3, 7:9, 11:14, 16:17), "hour"] == 13))
  expect_true(all(is.na(pttm_mat[-c(1, 3, 6:9, 11:14, 16:17), "hour"])))

  expect_true(all(pttm_mat[c(1:2, 6:10, 12:15, 17), "min"] == 15))
  expect_true(all(is.na(pttm_mat[-c(1:2, 6:10, 12:15, 17), "min"])))

  expect_true(all(pttm_mat[c(1, 3, 7:11, 13:16), "sec"] == 17))
  expect_true(all(is.na(pttm_mat[-c(1, 3, 7:11, 13:16), "sec"])))
})

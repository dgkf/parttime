test_that("parsing iso8601 week without weekday throws warning", {
  expect_warning(as.parttime("2022-W03"), "loss.*resolution")
  expect_silent(as.parttime("2022-W03", warn = FALSE))
})

test_that("parsing typical cdisc works using iso8601 parser", {
  pttms <- expect_silent(as.parttime(cdisc_datetimes_decreasing_sensitivity))
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

test_that("parsing invalid iso8601 with mixed formats throws warning and returns NAs", {
  # basic date, but extended time
  expect_true(is.na(expect_warning(as.parttime("20220101T01:02"), "parse")))
  expect_true(is.na(expect_warning(as.parttime("20220101T01:02:03"), "parse")))
  expect_true(is.na(expect_warning(as.parttime("20220101T01:02:03+07:00"), "parse")))

  # extended date, but basic time
  expect_true(is.na(expect_warning(as.parttime("2022-01-01T0102"), "parse")))
  expect_true(is.na(expect_warning(as.parttime("2022-01-01T010203"), "parse")))

  # extended date & time, but basic tz
  expect_true(is.na(expect_warning(as.parttime("2022-01-01T01:02:03+0700"), "parse")))

  # basic date & time, but extended tz
  expect_true(is.na(expect_warning(as.parttime("20220101T010203+07:00"), "parse")))
})

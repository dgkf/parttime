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

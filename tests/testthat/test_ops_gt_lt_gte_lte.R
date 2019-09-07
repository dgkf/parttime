context("parttime comparison operators")

test_that("comparison of same partially missing date is unknown", {
  options(parttime.assume_tz_offset = 0)
  
  expect_equal({
    as.logical(parttime(2019) < parttime(2019))
  }, {
    NA
  })
  
  expect_equal({
    as.logical(parttime(2019, 1) < parttime(2019, 2))
  }, {
    TRUE
  })
})

test_that("possibly() resolves uncertainty for overlapping windows", {
  options(parttime.assume_tz_offset = 0)
  expect_true(possibly(parttime(2019) < parttime(2019)))
  expect_true(possibly(parttime(2019, 1) < parttime(2019)))
  expect_true(possibly(parttime(2019) < parttime(2019, 1)))
  expect_false(possibly(parttime(2019, 2) < parttime(2019, 1)))
  
  # possibly takes into account wide timezone possibilities, which can cause
  # uncertainty in otherwise clear comparisons
  options(parttime.assume_tz_offset = NA)
  expect_true(is.na(getOption("parttime.assume_tz_offset")))
  expect_true(possibly(parttime(2019, 2) < parttime(2019, 1)))
})

test_that("definitely() excludes uncertainty for overlapping windows", {
  expect_false(definitely(parttime(2019) < parttime(2019)))
  expect_false(definitely(parttime(2019, 1) < parttime(2019)))
  expect_false(definitely(parttime(2019) < parttime(2019, 1)))
  expect_true(definitely(parttime(2019, 1) < parttime(2019, 3)))
})

test_that("timezone causes uncertainty in comparison", {
  expect_equal({
    as.logical(parttime(2019, tzhour = NA, tzmin = NA) < parttime(2019, tzhour = NA, tzmin = NA))
  }, {
    NA
  })
})

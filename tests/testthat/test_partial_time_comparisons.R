test_that("partial times can be compared with `min`, `max`", {
  x <- as.parttime("2017")
  y <- as.parttime("2017-01")
  expect_equal(max(x, y), as.parttime("2017"))

  x <- as.parttime("2017")
  y <- as.parttime("2017-01-01T01:02:03")
  expect_equal(max(x, y), as.parttime("2017"))

  x <- as.parttime("2017-03-04")
  y <- as.parttime("2017-01-01T01:02:03")
  expect_equal(max(x, y), as.parttime("2017-03-04"))
})

test_that("partial times extrema ignores missing values when na.rm is TRUE", {
  x <- as.parttime("2017")
  y <- as.parttime("2017-01")
  expect_equal(suppressWarnings(max(x, y, na.rm = TRUE)), as.parttime("2017-01"))

  x <- as.parttime("2017")
  y <- as.parttime("2017-01-01T01:02:03")
  expect_equal(suppressWarnings(max(x, y, na.rm = TRUE)), as.parttime("2017-01-01T01:02:03"))

  x <- as.parttime("2017-03-04")
  y <- as.parttime("2017-01-01T01:02:03")
  expect_equal(suppressWarnings(max(x, y, na.rm = TRUE)), as.parttime("2017-03-04"))
})

test_that("partial times extrema with na.rm TRUE warns unless na.warn is FALSE", {
  x <- as.parttime("2017")
  y <- as.parttime("2017-01")
  expect_warning(max(x, y, na.rm = TRUE))
  expect_silent(max(x, y, na.rm = TRUE, na.warn = FALSE))

  x <- as.parttime("2017")
  y <- as.parttime("2017-01-01T01:02")
  expect_warning(max(x, y, na.rm = TRUE))
  expect_silent(max(x, y, na.rm = TRUE, na.warn = FALSE))
})

test_that("partial times extrema with na.rm TRUE does not warn when a value exists for all fields", {
  withr::with_options(list(parttime.assume_tz_offset = 0L), {
    x <- as.parttime("2017")
    y <- as.parttime("2017-01-01T01:02:30")
    expect_silent(max(x, y, na.rm = TRUE))
    expect_silent(max(x, y, na.rm = TRUE, na.warn = FALSE))
  })
})



test_that("partial times vectorized extrema", {
  x <- as.parttime(c("2017", "2017-01-01"))
  y <- as.parttime(c("2017-01", "2017-01"))

  expect_equal(pmax(x, y), as.parttime(c("2017", "2017-01")))
  expect_equal(pmax(x, y, na.rm = TRUE), as.parttime(c("2017-01", "2017-01-01")))

  x <- as.parttime(c("2017", "2017-01-01"))
  y <- as.parttime(c("2017-01", "2017-01"))
  z <- as.parttime(c("2017-012", "2017-02-01"))

  expect_equal(pmax(x, y, z), as.parttime(c("2017", "2017-02-01")))
  expect_equal(pmax(x, y, z, na.rm = TRUE), as.parttime(c("2017-012", "2017-02-01")))
})

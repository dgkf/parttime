ymd <- function(x) unname(vctrs::field(x, "pttm_mat")[, c("year", "month", "day"), drop = FALSE])

test_that("base order() and sort() agree with vctrs", {
  x <- as.parttime(c("2015-04-13", "2015", "2015-04", "2014-12-31"))
  expect_equal(order(x), vctrs::vec_order(x))
  expect_equal(ymd(sort(x)), ymd(vctrs::vec_sort(x)))
})

test_that("ordering is by component, most significant first", {
  x <- as.parttime(c("2015-04-13", "2015", "2015-04", "2014-12-31"))
  # 2014-12-31 is earliest, then the 2015 values, a value saying more before
  # one saying less because an absent component sorts last.
  expect_equal(order(x), c(4L, 1L, 3L, 2L))
  expect_equal(
    ymd(sort(x)),
    matrix(
      c(2014, 12, 31,
        2015,  4, 13,
        2015,  4, NA,
        2015, NA, NA),
      nrow = 4, byrow = TRUE
    )
  )
})

test_that("a missing value ranks NA so sort() drops it", {
  x <- as.parttime(c("2015-04-13", NA, "2014-01-01"))
  expect_equal(is.na(xtfrm(x)), c(FALSE, TRUE, FALSE))
  expect_equal(length(sort(x)), 2L)
  expect_equal(ymd(sort(x)), matrix(c(2014, 1, 1, 2015, 4, 13), nrow = 2, byrow = TRUE))
  expect_equal(length(sort(x, na.last = TRUE)), 3L)
})

test_that("degenerate input", {
  expect_equal(xtfrm(as.parttime(character(0))), integer(0))
  expect_equal(xtfrm(as.parttime("2015")), 1L)
  expect_equal(length(sort(as.parttime(character(0)))), 0L)
})

test_that("c() concatenates and keeps the class", {
  a <- as.parttime("2015-04")
  b <- as.parttime(c("2016-08-01", "2014"))
  out <- c(a, b)
  expect_s3_class(out, "partial_time")
  expect_equal(length(out), 3L)
  expect_equal(
    ymd(out),
    matrix(c(2015, 4, NA, 2016, 8, 1, 2014, NA, NA), nrow = 3, byrow = TRUE)
  )
})

test_that("c() round-trips through a subset", {
  x <- as.parttime(c("2015", "2015-04", "2015-04-13"))
  expect_equal(ymd(c(x[1], x[3])), ymd(x[c(1, 3)]))
})

context("parttime dplyr::case_when integration tests")

library(dplyr)

test_that("parttime works when used in the context of a dplyr::case_when", {
  x <- as.parttime(c("2001", "1984", "3030-01-02", ""))

  expect_equal({
    dplyr::case_when(
      c(TRUE, FALSE, TRUE, FALSE) ~ x,
      FALSE ~ x,
      TRUE ~ x)
  }, {
    x
  })
})
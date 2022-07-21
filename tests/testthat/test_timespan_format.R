test_that("format.timespan properly indicates entirely abscent timespans", {
  expect_silent(tmspn <- as.timespan(""))
  expect_silent(fmt <- crayon::strip_style(format(tmspn)))
  expect_equal(fmt, "NA")
})

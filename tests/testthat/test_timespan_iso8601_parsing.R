test_that("parsing iso8601 week without weekday produces timespan", {
  expect_silent(tmspn <- as.timespan(iso8601_dates[[5]]))

  expect_equal(month(start(tmspn)), 3)
  expect_equal(day(start(tmspn)), 28)

  expect_equal(month(end(tmspn)), 4)
  expect_equal(day(end(tmspn)), 4)

  expect_silent(tmspn <- as.timespan("2022-W06"))

  expect_equal(day(start(tmspn)), 6)
  expect_equal(day(end(tmspn)), 13)
  expect_equal(day(end(tmspn)) - day(start(tmspn)), 7)
})

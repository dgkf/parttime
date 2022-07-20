test_that("parsing iso8601 week without weekday throws warning", {
  expect_warning(as.parttime("2022-W03"), "loss.*resolution")
  expect_silent(as.parttime("2022-W03", warn = FALSE))
})

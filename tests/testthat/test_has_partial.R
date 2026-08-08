test_that("has_partial() works with its default components", {
  x <- as.parttime(c("2015", "2015-04-13T10:30:15"))
  expect_equal(unname(has_partial(x)), c(TRUE, FALSE))
})

test_that("has_partial_date() and has_partial_time() work", {
  expect_equal(
    unname(has_partial_date(as.parttime(c("2015-04", "2015-04-13")))),
    c(TRUE, FALSE)
  )
  expect_equal(
    unname(has_partial_time(as.parttime(c("2015-04-13", "2015-04-13T10:30:15")))),
    c(TRUE, FALSE)
  )
})

test_that("every default component is one the object carries", {
  # The defaults used to name `secfrac` and `tzmin`, which `partial_time()`
  # does not build, so every call with defaults was a subscript error.  Reading
  # the names off the object is what stops the two lists diverging again.
  x <- as.parttime("2015-04-13T10:30:15")
  carried <- colnames(vctrs::field(x, "pttm_mat"))
  for (component in carried) {
    expect_type(has_partial(x, components = component), "logical")
  }
})

test_that("an unknown component is refused by name", {
  x <- as.parttime("2015-04-13")
  expect_error(has_partial(x, components = "secfrac"), "secfrac")
  expect_error(has_partial(x, components = "tzmin"), "tzmin")
})

test_that("components can still be given positionally", {
  x <- as.parttime("2015-04")
  expect_equal(unname(has_partial(x, "year", "month")), FALSE)
  expect_equal(unname(has_partial(x, "day")), TRUE)
})

test_that("degenerate input", {
  expect_equal(unname(has_partial(as.parttime(character(0)))), logical(0))
  expect_equal(unname(has_partial(as.parttime(NA_character_))), TRUE)
})

context("parttime dplyr::mutate integration tests")

library(dplyr)

test_that("parttime works when used in the context of a dplyr::mutate following dplyr 0.9.0", {
  x <- c("2001", "1984", "3030-01-02", "")

  expect_equal({
    if (packageVersion("dplyr") >= "0.9.0") {
      #' vctrs integration will happen in 0.9.0
      #' https://github.com/tidyverse/dplyr/issues/2432
      dplyr::tibble(x = x) %>%
        mutate(y = as.parttime(x)) %>%
        pull(y)
    } else {
      dplyr::tibble(x = x) %>%
        { .$y <- as.parttime(.$x); . } %>%
        pull(y)
    }
  }, {
      dplyr::tibble(x = x, y = as.parttime(x)) %>%
        pull(y)
  })
})

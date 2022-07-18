#' Propegate field missingness from higher to lower resolution
#'
#' @param x a vector of parttime objects with days which may exceed viable days
#'   of month
#' @param keep_tz logical indicating whether to retain timezone fields, even if
#'   other fields are missing
#'
#' @return a vector of parttime objects with missingness propagated to lower
#'   resolution time fields
#'
#' @examples
#' x <- as.parttime(c("2019", "2019-02-31", "2019-01-05", "2016-02-31",
#'   "2016-01-05", "2020-01-01 03:04:05.1234"))
#' vctrs::field(x, "pttm_mat")[,"min"] <- 23
#' parttime:::propagate_na(x)
#'
propagate_na <- function(x, keep_tz = FALSE) {
  vctrs::field(x, "pttm_mat") <- propagate_na_matrix(
    vctrs::field(x, "pttm_mat"),
    keep_tz = keep_tz)

  x
}



propagate_na_matrix <- function(x, keep_tz = FALSE) {
  cols <- grepl("^tz", colnames(x))

  # if not keeping tz fixed, propagate tz uncertainty back up through values
  x_pos_na <- apply(x, 1, Position, f = is.na)
  subset_of_na <- col(x) >= x_pos_na
  if (keep_tz) {
    subset_of_na[, cols] <- FALSE
  } else {
    tz_na <- apply(is.na(x[, cols, drop = FALSE]), 1, any)
    subset_of_na[tz_na, ] <- col(x[tz_na, , drop = FALSE]) + 1 >= x_pos_na
  }

  # only propagate to tz fields if tz
  x[subset_of_na] <- NA
  x
}

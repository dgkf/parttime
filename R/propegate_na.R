#' Propegate field missingness from higher to lower resolution
#'
#' @param x a vector of parttime objects with days which may exceed viable days
#'   of month
#' @param keep_tz logical indicating whether to retain timezone fields, even if
#'   other fields are missing
#'
#' @return a vector of parttime objects with missingness propegated to lower
#'   resolution time fields
#'
#' @examples
#' x <- as.parttime(c("2019", "2019-02-31", "2019-01-05", "2016-02-31", 
#'   "2016-01-05", "2020-01-01 03:04:05.1234"))
#' vctrs::field(x, "pttm_mat")[,"min"] <- 23
#' parttime:::propegate_na(x)
#'
propegate_na <- function(x, keep_tz = FALSE) {
  vctrs::field(x, "pttm_mat") <- propegate_na_matrix(
    vctrs::field(x, "pttm_mat"), 
    keep_tz = keep_tz)
  
  x
}



propegate_na_matrix <- function(x, keep_tz = FALSE) {
  cols <- grepl("^tz", colnames(x))
  subset_of_na <- col(x) >= apply(x, 1, Position, f = is.na)

  # only propegate to tz fields if tz
  if (keep_tz) subset_of_na[,cols] <- FALSE
  x[subset_of_na] <- NA
  x
}

#' Normalize days in month back to day limit for a given month
#'
#' @param x a vector of parttime objects with days which may exceed viable days
#'   of month
#'
#' @return a vector of parttime objects with normalized days of the month
#'
#' @examples
#' x <- as.parttime(c("2019", "2019-02-31", "2019-01-05", "2016-02-31", "2016-01-05"))
#' parttime:::normalize_month_day(x)
#'
#' @importFrom lubridate days_in_month leap_year
#'
normalize_month_day <- function(x) {
  month_days <- lubridate::days_in_month(vctrs::field(x, "pttm_mat")[,"month"])
  leap_febs <- which(
    lubridate::leap_year(vctrs::field(x, "pttm_mat")[,"year"]) &
    vctrs::field(x, "pttm_mat")[,"month"] == 2)

  month_days[leap_febs] <- month_days[leap_febs] + 1
  exceeded <- which(vctrs::field(x, "pttm_mat")[,"day"] > month_days)
  vctrs::field(x, "pttm_mat")[exceeded,"day"] <- month_days[exceeded]
  x
}

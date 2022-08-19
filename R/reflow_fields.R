#' Reflow potentially invalid time components to adjacent fields
#'
#' @param fmat a fields matrix as part of a partial_time or partial_difftime
#' @param days a logical indicating whether year and month should be consolidated
#'   into total days. If an integer is provided, days should represent the
#'   "leap-time" to add on top of non-leap conversion.
#'
#' @return a fields matrix with appropriately ranged time components
#' @examples
#'
#' # example with difftimes (when you only care about days of change)
#'
#' x <- as.parttime("2019-06-23 04:33:21.123")
#' y <- as.parttime("2018-02-08 12:59:28.987")
#'
#' diff_fields <- vctrs::field(x, "pttm_mat") - vctrs::field(y, "pttm_mat")
#'
#' parttime:::reflow_fields(diff_fields)
#'
#' # if we want to assume 0.25 leap days per year
#' parttime:::reflow_fields(diff_fields, days = TRUE)
#'
#' # if we want to assert that there were no leap days
#' parttime:::reflow_fields(diff_fields, days = 0)
#'
reflow_fields <- function(fmat, days) {
  # account for leap day (1/4 of a day per year) prior to reflowing
  if (!missing(days) && isTRUE(days))
    fmat[, "hour"] <- fmat[, "hour"] + fmat[, "year"] * 6

  # flow decimal places down to subsequent fields
  fmat[, "hour"] <- fmat[, "hour"] + fmat[, "day"] %% 1 * 24
  fmat[, "day"] <- fmat[, "day"] %/% 1
  fmat[, "min"] <- fmat[, "min"] + fmat[, "hour"] %% 1 * 60
  fmat[, "hour"] <- fmat[, "hour"] %/% 1
  fmat[, "sec"] <- fmat[, "sec"] + fmat[, "min"] %% 1 * 60
  fmat[, "min"] <- fmat[, "min"] %/% 1

  # flow overflow back up to parent fields
  na <- is.na(fmat[, "sec"])
  fmat[!na, "min"] <- fmat[!na, "min"] + fmat[!na, "sec"] %/% 60
  fmat[, "sec"] <- fmat[, "sec"] %% 60
  na <- is.na(fmat[, "min"])
  fmat[!na, "hour"] <- fmat[!na, "hour"] + fmat[!na, "min"] %/% 60
  fmat[, "min"] <- fmat[, "min"] %% 60
  na <- is.na(fmat[, "hour"])
  fmat[!na, "day"] <- fmat[!na, "day"] + fmat[!na, "hour"] %/% 24
  fmat[, "hour"] <- fmat[, "hour"] %% 24

  if (!missing(days)) {
    # if consolidating down to days, flatten years and months

    fmat[, "year"] <- fmat[, "year"] + (fmat[, "month"] - 1) %/% 12
    fmat[, "month"] <- (fmat[, "month"] - 1) %% 12 + 1

    y_m_na <- is.na(fmat[, "year"]) | is.na(fmat[, "month"])

    fmat[!y_m_na, "day"] <- fmat[!y_m_na, "day"] +
      ifelse(is.numeric(days), days, 0) +
      fmat[!y_m_na, "year"] * 365 +
      c(0, cumsum(lubridate::days_in_month(1:11)))[fmat[!y_m_na, "month"]]

    fmat[, c("year", "month")] <- NA
  } else {
    # otherwise continue to reflow into years and months

    mds <- month_days(fmat)

    na <- is.na(fmat[, "day"])
    fmat[!na, "month"] <- fmat[!na, "month"] + (fmat[!na, "day"] - 1) %/% mds[!na]
    fmat[!na, "day"] <- (fmat[!na, "day"] - 1) %% mds[!na] + 1

    na <- is.na(fmat[, "month"])
    fmat[!na, "year"] <- fmat[!na, "year"] + (fmat[!na, "month"] - 1) %/% 12
    fmat[, "month"] <- (fmat[, "month"] - 1) %% 12 + 1
  }

  # TODO:
  #   Need to handle case where days affects more than +/-1 month for situation
  #   when adding partial_difftime to partial_time

  fmat
}



month_days <- function(x) {
  ifelse(lubridate::leap_year(x[, "year"]) & x[, "month"] %% 12 == 2, 1, 0) +
    lubridate::days_in_month((x[, "month"] - 1) %% 12 + 1)
}

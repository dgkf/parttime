#' Check if elements of a partial time vector is NA
#'
#' @param x partial_time vector to test
#' @param ... additional arguments unused
#'
#' @return A logical vector indicating whether each element in the
#'   `partial_time` vector is `NA`.
#'
#' @export
is.na.partial_time <- function(x, ...) {
  # A UTC offset says how to read a time, not that one was recorded, and it is
  # filled from `parttime.assume_tz_offset` when the value carried none.
  # Counting it would let a value with no date and no time report as present on
  # the strength of an assumption.
  mat <- vctrs::field(x, "pttm_mat")
  datetime <- setdiff(colnames(mat), "tzhour")
  unname(apply(is.na(mat[, datetime, drop = FALSE]), 1, all))
}



#' Shorthand for checking partial time inheritance
#'
#' @param x object to test
#'
#' @return A logical scalar indicating whether an object is a `partial_time`
#'   object.
#'
#' @rdname is_parttime
#' @family is_parttime
#' @export
is_partial_time <- function(x) {
  inherits(x, "partial_time")
}



#' @rdname is_parttime
#' @export
is.partial_time <- is_partial_time



#' @rdname is_parttime
#' @export
is_parttime <- is_partial_time



#' @rdname is_parttime
#' @export
is.parttime <- is_partial_time

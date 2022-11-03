#' Check if elements of a partial time vector is NA
#'
#' @param x partial_time vector to test
#' @param ... additional arguments unused
#'
#' @return A logical vector indicating whether each element in the
#'   `partial_timespan` vector is `NA`.
#'
#' @export
is.na.timespan <- function(x, ...) {
  cols <- head(dimnames(vctrs::field(x, "tmspn_arr"))[[2L]], -1L)
  unname(apply(is.na(vctrs::field(x, "tmspn_arr")[, cols, , drop = FALSE]), 1, all))
}



#' Shorthand for checking timespan inheritance
#'
#' @param x object to test
#'
#' @return A logical scalar indicating whether an object is a `partial_timespan`
#'   object.
#'
#' @export
is_timespan <- function(x) {
  inherits(x, "timespan")
}



#' @inherit is_timespan
#' @export
is.timespan <- is_timespan

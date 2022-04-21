#' Check if elements of a partial time vector is NA
#'
#' @param x partial_time vector to test
#' @param ... additional arguments unused
#'
#' @export
is.na.partial_time <- function(x, ...) {
  unname(apply(is.na(vctrs::field(x, "pttm_mat")), 1, all))
}



#' Shorthand for checking partial time inheritance
#'
#' @param x object to test
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

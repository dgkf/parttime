#' @export
is.na.partial_time <- function(x, ...) {
  unname(apply(is.na(vctrs::field(x, "pttm_mat")), 1, all))
}



#' @export
is_partial_time <- function(x) {
  inherits(x, "partial_time")
}



#' @export
is.partial_time <- is_partial_time



#' @export
is.parttime <- is_partial_time



#' @export
is_parttime <- is_partial_time

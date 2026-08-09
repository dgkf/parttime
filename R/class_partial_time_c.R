#' Concatenate partial_time objects
#'
#' @param ... `partial_time` objects to concatenate
#'
#' @return A `partial_time` object.
#'
#' @examples
#' c(as.parttime("2015-04"), as.parttime("2016-08-01"))
#'
#' @export
c.partial_time <- function(...) {
  # `vctrs::vec_c()` is the concatenation the record type already defines, and
  # it is what the coercion methods in this package are written against.
  vctrs::vec_c(...)
}

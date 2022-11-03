#' parttime vector dimensions
#'
#' @param x A `partial_time` object
#'
#' @return An `integer` vector of dimensions (length) of a `partial_time` vector
#'
#' @export
dim.partial_time <- function(x) { length(x) }

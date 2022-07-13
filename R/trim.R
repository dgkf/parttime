#' Shorten a timespan
#'
#' @param x timespan object to trim
#' @param ... additional arguments passed on to functions
#'
trim <- function(x, ...) {
  UseMethod("trim")
}

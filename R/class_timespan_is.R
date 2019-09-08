#' Shorthand for checking timespan inheritance 
#' 
#' @param x object to test
#' 
#' @export
is_timespan <- function(x) {
  inherits(x, "timespan")
}



#' @inherit is_timespan
#' @export
is.timespan <- is_timespan

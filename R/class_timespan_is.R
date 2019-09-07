#' @export
is_timespan <- function(x) {
  inherits(x, "timespan")
}



#' @export
is.timespan <- is_timespan

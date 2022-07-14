#' Cast an object to a timespan
#'
#' @param x an object to cast
#' @export
as.timespan <- function(x) {
  UseMethod("as.timespan")
}



#' @export
as.timespan.default <- function(x) {
  vctrs::vec_cast(x, structure(0L, class = "timespan"))
}



#' Cast to timespan object
#'
#' @inheritParams vctrs::vec_cast
#' @importFrom vctrs vec_cast
#' @exportS3Method vec_cast timespan
vec_cast.timespan <- function(x, to, ...) {
  if (is.timespan(x)) return(x)
  UseMethod("vec_cast.timespan")
}



#' Default handler for casting to a timespan
#'
#' @inheritParams vctrs::vec_cast
#' @importFrom vctrs stop_incompatible_cast vec_recycle
#' @exportS3Method vec_cast.timespan default
vec_cast.timespan.default <- function(x, to, ...) {
  if (!all(is.na(x) | is.null(x))) vctrs::stop_incompatible_cast(x, to)
  vctrs::vec_recycle(timespan(NA), size = length(x))
}



#' Cast partial time to timespan, representing uncertainty as a range
#'
#' @inheritParams vctrs::vec_cast
#' @exportS3Method vec_cast.timespan partial_time
vec_cast.timespan.partial_time <- function(x, to, ...) {
  timespan(
    x,
    minimally_increment(x),
    inclusive = c(TRUE, FALSE)
  )
}



#' Cast an array to a timespan
#'
#' @inheritParams vctrs::vec_cast
#' @exportS3Method vec_cast.timespan numeric
vec_cast.timespan.numeric <- function(x, to, ...) {
  vctrs::new_rcrd(
    fields = list(tmspn_arr = x),
    class = "timespan"
  )
}



#' Cast an array to a timespan
#'
#' @inheritParams vctrs::vec_cast
#' @exportS3Method vec_cast.timespan double
vec_cast.timespan.double <- vec_cast.timespan.numeric

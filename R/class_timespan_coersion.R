#' Cast an object to a timespan 
#' 
#' @param x an object to cast
#'
#' @export
as.timespan <- function(x) {
  UseMethod("as.timespan")
}



#' @export
as.timespan.default <- function(x) {
  vctrs::vec_cast(x, structure(0L, class = "timespan"))
}



#' Wrapper for lubridate as.interval
#' 
#' @inherit lubridate::as.interval
#' 
#' @importFrom methods setMethod signature
#' @importFrom lubridate as.interval
#' @export
methods::setMethod("as.interval", methods::signature("ANY"), function(x, ...) {
  # attempt to play nicely with lubridate precedent
  tryCatch({
    vctrs::vec_cast(x, structure(0L, class = "timespan"))
  }, error = function(e) {
    if (inherits(e, "vctrs_error_incompatible_cast"))
      lubridate::as.interval(x, ...)
    else
      stop(e)
  })
})



#' Cast to timespan object
#' 
#' @inheritParams vctrs::vec_cast
#' 
#' @export
vec_cast.timespan <- function(x, to, ...) {
  if (is.timespan(x)) return(x)
  UseMethod("vec_cast.timespan")
}



#' Default handler for casting to a timespan
#' 
#' @inheritParams vctrs::vec_cast
#' 
#' @importFrom vctrs stop_incompatible_cast
#' @export
vec_cast.timespan.default <- function(x, to, ...) {
  if (!all(is.na(x) | is.null(x))) vctrs::stop_incompatible_cast(x, to)
  vctrs::vec_recycle(timespan(NA), size = length(x))
}



#' Cast partial time to timespan, representing uncertainty as a range
#' 
#' @inheritParams vctrs::vec_cast
#' 
#' @export
vec_cast.timespan.partial_time <- function(x, to, ...) {
  xdim <- dim(vctrs::field(x, "pttm_mat"))
  xdimnames <- dimnames(vctrs::field(x, "pttm_mat"))
  
  timespan(
    x,
    minimally_increment(x),
    inclusive = c(TRUE, FALSE))
}



#' Cast an array to a timespan
#' 
#' @inheritParams vctrs::vec_cast
#' 
#' @export
vec_cast.timespan.array <- function(x, to, ...) {
  vctrs::new_rcrd(
    fields = list(tmspn_arr = x),
    class = "timespan")
}
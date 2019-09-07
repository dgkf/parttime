#' @export
as.timespan <- function(x) {
  UseMethod("as.timespan")
}



#' @export
as.timespan.default <- function(x) {
  vctrs::vec_cast(x, structure(0L, class = "timespan"))
}



#' @importFrom lubridate as.interval
#' @export
setMethod("as.interval", signature("ANY"), function(x, ...) {
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



#' @export
vec_cast.timespan <- function(x, to, ...) {
  if (is.timespan(x)) return(x)
  UseMethod("vec_cast.timespan")
}



#' @export
vec_cast.timespan.default <- function(x, to, ...) {
  if (!all(is.na(x) | is.null(x))) vctrs::stop_incompatible_cast(x, to)
  vctrs::vec_recycle(timespan(NA), size = length(x))
}



#' @export
vec_cast.timespan.partial_time <- function(x, to, ...) {
  xdim <- dim(vctrs::field(x, "pttm_mat"))
  xdimnames <- dimnames(vctrs::field(x, "pttm_mat"))
  
  timespan(
    x,
    minimally_increment(x),
    inclusive = c(TRUE, FALSE))
}



#' @export
vec_cast.timespan.array <- function(x, to, ..., inclusive = TRUE) {
  inclusive <- matrix(inclusive, nrow = nrow(x), ncol = 2, byrow = TRUE)
  
  vctrs::new_rcrd(
    fields = list(tmspn_arr = x),
    class = "timespan")
}
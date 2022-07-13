#' This class is purely for retaining logical information for immediate
#' truncation as to not overccomplicate coersion needs with another vctr class
#'
#' @param x create a parttime logical to store logical matrix following parttime
#'   comparison operations
#'
parttime_logical <- function(x) {
  structure(unname(x[, ncol(x)]),
    pttm_lgl = x,
    class = c("partial_time_logical", "logical"))
}



validate_res <- function(x, res) {
  if (is.character(res))
    res <- match(res, colnames(attr(x, "pttm_lgl")))

  if (!is.finite(res) | res < 1 | res > ncol(attr(x, "pttm_lgl")))
    stop("res must be a valid pttm_lgl matrix column index")

  res
}



#' @export
as.logical.partial_time_logical <- function(x, ...,
    res = ncol(attr(x, "pttm_lgl"))) {
  res <- validate_res(x, res)
  unname(attr(x, "pttm_lgl")[, res])
}



#' @export
print.partial_time_logical <- function(x, ...) {
  print(as.logical(x))
}



#' @export
format.partial_time_logical <- function(x, ...) {
  format(as.logical(x))
}

#' Determine whether one object includes another
#'
#' @param e1 object to test whether includes \code{e2}
#' @param e2 object to test whether included in \code{e1}
#'
#' @return A logical vector indicating whether `e1` includes `e2`
#'
#' @export
includes <- function(e1, e2) {
  UseMethod("includes")
}



#' Determine whether a partial time contains an object
#'
#' @inheritParams includes
#' @inherit includes return
#'
#' @method includes partial_time
#' @export
includes.partial_time <- function(e1, e2) {
  UseMethod("includes.partial_time", e2)
}



#' Test for whether a timestamp could be included within parttime uncertainty
#'
#' @inheritParams includes
#'
#' @return A logical vector indicating whether `partial_time` `e1` includes
#'   `partial_time` `e2`
#'
#' @examples
#' x_chr <- c("2019", "2019-03-01", "2019-03",    "2018",    "",     "2018", "")
#' y_chr <- c("2019", "2019-03",    "2019-03-01", "2016-05", "2018", "",     "")
#'
#' x <- as.parttime(x_chr)
#' y <- as.parttime(y_chr)
#'
#' includes(x, y)
#'
#' @method includes.partial_time partial_time
#' @export
includes.partial_time.partial_time <- function(e1, e2) {
  es <- vctrs::vec_recycle_common(e1, e2)
  e1f <- vctrs::field(es[[1]], "pttm_mat")
  e2f <- vctrs::field(es[[2]], "pttm_mat")
  unname(!apply(e1f != e2f | (!is.na(e1f) & is.na(e2f)), 1, any, na.rm = TRUE))
}



includes.timespan <- function(e1, e2) {
  UseMethod("includes.timespan")
}

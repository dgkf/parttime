#' @export
includes <- function(e1, e2) {
  UseMethod("includes")
}



#' @export 
includes.partial_time <- function(e1, e2) {
  UseMethod("includes.partial_time", e2)
}



#' Test for whether a timestamp could be included within parttime uncertainty
#'
#' @export
#' @examples
#' x <- as.parttime(c("2019", "2019-03-01", "2019-03",    "2018",    "",     "2018", ""))
#' y <- as.parttime(c("2019", "2019-03",    "2019-03-01", "2016-05", "2018", "",     ""))
#'
#' includes(x, y)
#'
includes.partial_time.partial_time <- function(e1, e2) {
  es <- vctrs::vec_recycle_common(e1, e2)
  e1f <- vctrs::field(es[[1]], "pttm_mat")
  e2f <- vctrs::field(es[[2]], "pttm_mat")
  unname(!apply(e1f != e2f | (!is.na(e1f) & is.na(e2f)), 1, any, na.rm = TRUE))
}



includes.timespan <- function(e1, e2) {
  UseMethod("includes.timespan")
}
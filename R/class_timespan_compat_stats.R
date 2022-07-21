start <- function(x, ...) {
  UseMethod("start")
}

#' @importFrom utils head
#' @rawNamespace S3method(stats::start,timespan)
#' @export
start.timespan <- function(x, ...) {
  cols <- utils::head(dimnames(vctrs::field(x, "tmspn_arr"))[[2]], -1L)
  dns <- dimnames(vctrs::field(x, "tmspn_arr"))
  as.parttime(array(
    vctrs::field(x, "tmspn_arr")[, cols, "lb"],
    dim = c(length(x), length(cols)),
    dimnames = list(dns[[1L]], cols)
  ))
}

end <- function(x, ...) {
  UseMethod("end")
}

#' @importFrom utils head
#' @rawNamespace S3method(stats::end,timespan)
#' @export
end.timespan <- function(x, ...) {
  cols <- utils::head(dimnames(vctrs::field(x, "tmspn_arr"))[[2]], -1L)
  dns <- dimnames(vctrs::field(x, "tmspn_arr"))
  as.parttime(array(
    vctrs::field(x, "tmspn_arr")[, cols, "ub"],
    dim = c(length(x), length(cols)),
    dimnames = list(dns[[1L]], cols)
  ))
}

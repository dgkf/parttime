#' @export
rep.partial_time <- function(x, ..., length.out) {
  if (!missing(length.out))
    return(rep_len.partial_time(x, length.out))
  vctrs::vec_repeat(x, ...)
}



rep_len.partial_time <- function(x, length.out) {
  x[rep_len(vctrs::vec_seq_along(x), length.out = length.out)]
}


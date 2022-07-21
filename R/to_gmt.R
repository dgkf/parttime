#' Generic for coercing timestamps to GMT timezone
#'
#' @param x object to coerce to GMT time
#'
#' @export
to_gmt <- function(x) {
  UseMethod("to_gmt")
}



#' @export
to_gmt.matrix <- function(x) {
  x[, "hour"] <- x[, "hour"] + x[, "tzhour"]
  x[, "min"]  <- x[, "min"]  + x[, "tzmin"]
  x[, c("tzhour", "tzmin")] <- 0
  reflow_fields(x)
}



#' @export
to_gmt.array <- function(x) {
  array(apply(x, 3, to_gmt), dim = dim(x), dimnames = dimnames(x))
}



#' @export
to_gmt.partial_time <- function(x) {
  vctrs::field(x, "pttm_mat") <- to_gmt(vctrs::field(x, "pttm_mat"))
  x
}



#' @export
to_gmt.timespan <- function(x) {
  vctrs::field(x, "tmspn_arr") <- to_gmt(vctrs::field(x, "tmspn_arr"))
  x
}



prune_tz <- function(x) {
  args <- rep_len(alist(, ), length(dim(x)))
  args[[2]] <- -which(dimnames(x)[[2]] %in% c("tzhour", "tzmin"))
  do.call("[", append(list(x), args))
}

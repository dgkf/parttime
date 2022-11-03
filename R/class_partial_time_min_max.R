min_max_handler <- function(f, ..., na.rm = FALSE, na.warn = TRUE) {
  dots <- lapply(list(...), as.parttime)
  x <- do.call(rbind, lapply(dots, vctrs::field, "pttm_mat"))

  x <- min_max_parttime_matrix_list(f, x, na.rm = na.rm, na.warn = na.warn)

  out <- parttime(NA)
  vctrs::field(out, "pttm_mat") <- x[1, , drop = FALSE]
  out
}



min_max_parttime_matrix_list <- function(f, x, na.rm = FALSE, na.warn = TRUE) {
  # if entire field column is NA, warn about method of handling
  if (na.rm && na.warn && any(all_na <- apply(apply(x, 2, is.na), 2, all))) {
    warning("no non-missing arguments for parttime fields ",
      spoken_list(colnames(x)[all_na], quote = TRUE),
      "; leaving as NA.")
    x[, all_na] <- NA
  }

  # work our way down time resolution scale, filtering down for max values
  f_handler <- function(i) i == f(i, na.rm = na.rm) | (!na.rm & is.na(i))
  for (i in seq_len(ncol(x))) {
    max_mask_i <- f_handler(x[, i])
    max_mask_i[is.na(max_mask_i)] <- FALSE
    x <- x[max_mask_i, , drop = FALSE]
    if (nrow(x) == 1) break
  }

  x[1, , drop = FALSE]
}



#' Get the maximum of a parttime vector
#'
#' @param ... partial_time objects
#' @param na.rm whether \code{NA} should be removed when calculating max
#' @param na.warn whether to raise a warning for \code{NA}
#'
#' @return A `partial_time` scalar
#'
#' @examples
#' max(parttime(c("2019", "2018", "2019-02", "2018-03")))
#'
#' @export
max.partial_time <- function(..., na.rm = FALSE, na.warn = TRUE) {
  min_max_handler(max, ..., na.rm = na.rm, na.warn = na.warn)
}



#' Get the minimum of aparttime vector
#'
#' @param ... partial_time objects
#' @param na.rm whether \code{NA} should be removed when calculating min
#' @param na.warn whether to raise a warning for \code{NA}
#'
#' @return A `partial_time` scalar
#'
#' @export
min.partial_time <- function(..., na.rm = FALSE, na.warn = TRUE) {
  min_max_handler(min, ..., na.rm = na.rm, na.warn = na.warn)
}



pmin_pmax_handler <- function(f, ..., na.rm = FALSE) {
  dots <- lapply(list(...), as.parttime)

  max_nrows <- max(dot_nrow <- sapply(dots, nrow))
  if (any(max_nrows %% dot_nrow != 0))
    warning("an argument will be fractionally recycled")

  dots <- lapply(dots, rep, length.out = max_nrows)
  out <- rep_len(parttime(NA), max_nrows)

  for (i in seq_along(out)) {
    vals <- lapply(dots, `[[`, i)
    out[[i]] <- do.call(f, append(vals, list(na.rm = na.rm, na.warn = FALSE)))
  }

  out
}



#' @inherit base::pmax
#' @export
pmax <- function(..., na.rm = FALSE) {
  UseMethod("pmax")
}



#' @export
pmax.default <- function(..., na.rm = FALSE) {
  base::pmax(..., na.rm = na.rm)
}



#' Get the elementwise maximum of parttime vectors
#'
#' @inheritParams base::pmax
#'
#' @return A `partial_time` vector with length equal to the maximum length of
#'   the vectors provided where each value is the maximum of the recycled values
#'   of each vector argument.
#'
#' @examples
#' pmax(
#'   parttime(c("2019", "2018", "2019-02", "2018",    "2010")),
#'   parttime(c("2020", NA,     "2019-03", "2018-01", "2010"))
#' )
#'
#' @export
pmax.partial_time <- function(..., na.rm = FALSE) {
  pmin_pmax_handler(base::max, ..., na.rm = na.rm)
}



#' @inherit base::pmin
#' @export
pmin <- function(..., na.rm = FALSE) {
  UseMethod("pmin")
}



#' @export
pmin.default <- function(..., na.rm = FALSE) {
  base::pmin(..., na.rm = na.rm)
}



#' Get the elementwise minimum of parttime vectors
#'
#' @inheritParams base::pmin
#'
#' @return A `partial_time` vector with length equal to the maximum length of
#'   the vectors provided where each value is the minimum of the recycled values
#'   of each vector argument.
#'
#' @export
pmin.partial_time <- function(..., na.rm = FALSE) {
  pmin_pmax_handler(base::min, ..., na.rm = na.rm)
}

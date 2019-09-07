min_max_handler <- function(f, ..., na.rm = FALSE, na.warn = TRUE) {
  dots <- lapply(list(...), as.parttime)
  x <- do.call(rbind, lapply(dots, vctrs::field, "pttm_mat"))
  
  x <- min_max_parttime_matrix_list(f, x, na.rm = na.rm, na.warn = na.warn)
  
  out <- parttime(NA)
  vctrs::field(out, "pttm_mat") <- x[1,, drop = FALSE]
  out
}



min_max_parttime_matrix_list <- function(f, x, na.rm = FALSE, na.warn = TRUE) {
  # if entire field column is NA, warn about method of handling
  if (na.rm && na.warn && any(all_na <- apply(apply(x, 2, is.na), 2, all))) {
    warning("no non-missing arguments for parttime fields ",
      spoken_list(colnames(x)[all_na], quote = TRUE),
      "; leaving as NA.")
    x[,all_na] <- NA
  }

  # work our way down time resolution scale, filtering down for max values
  f_handler <- function(i) i == f(i, na.rm = na.rm) | (!na.rm & is.na(i))
  for (i in seq_len(ncol(x))) {
    max_mask_i <- f_handler(x[,i])
    max_mask_i[is.na(max_mask_i)] <- FALSE
    x <- x[max_mask_i,, drop = FALSE]
    if (nrow(x) == 1) break
  }
  
  x[1,, drop = FALSE]
}



#' @examples
#' max(parttime(c("2019", "2018", "2019-02", "2018-03")))
#'
#' @export
max.partial_time <- function(..., na.rm = FALSE, na.warn = TRUE) {
  min_max_handler(max, ..., na.rm = na.rm, na.warn = na.warn)
}



#' @export
min.partial_time <- function(..., na.rm = FALSE, na.warn = TRUE) {
  min_max_handler(min, ..., na.rm = na.rm, na.warn = na.warn)
}



pmin_pmax_handler <- function(f, ..., na.rm = FALSE) {
  # TODO:
  #   handle case where x's are all length 1
  dots <- lapply(list(...), as.parttime)

  max_nrows <- max(dot_nrow <- sapply(dots, nrow))
  if (any(max_nrows %% dot_nrow != 0))
    warning("an argument will be fractionally recycled")

  dots <- lapply(dots, rep, length.out = max_nrows)

  x <- lapply(dots, vctrs::field, "pttm_mat")
  x <- if (length(x) == 1) x[[1]] 
  else pmin_pmax_parttime_matrix_list(f, x, na.rm = na.rm)
  
  colnames(x) <- colnames(vctrs::field(dots[[1]], "pttm_mat"))
  out <- rep(parttime(NA), length.out = nrow(x))
  vctrs::field(out, "pttm_mat") <- x
  out <- propegate_na(out, keep_tz = TRUE)
  out
}



pmin_pmax_parttime_matrix_list <- function(f, x, na.rm = FALSE) {
  dim_in <- dim(x[[1]])
  colnames_in <- colnames(x[[1]])
  rownames_in <- rownames(x[[1]])

  x <- array(do.call(rbind, x), dim = c(dim_in[[1]], length(x), dim_in[[2]]))

  # work our way down time resolution scale, filtering down for max values
  f_handler <- function(i) i == f(i, na.rm = na.rm) | (!na.rm & is.na(i))
  max_mask <- array(TRUE, dim = dim(x)[c(1, 2)])

  for (i in seq_len(dim(x)[[3]])) {
    x[,,i][!max_mask] <- NA
    max_x_i <- x[,, i, drop = FALSE]
    if (sum(is.na(max_x_i)) == prod(dim(max_x_i))) break
    max_x_i[rowSums(is.na(max_x_i)) == ncol(max_x_i),,1] <- 0
    max_x_i <- t(apply(max_x_i, 1, f_handler))
    max_mask <- max_mask & max_x_i
  }

  # coerce back into standard matrix in long form and subset based on mask
  x <- matrix(x, ncol = dim_in[[2]])
  x <- x[seq_len(dim_in[[1]]) + dim_in[[1]] * (apply(max_mask, 1, Position, f = isTRUE) - 1),, drop = FALSE]
  colnames(x) <- colnames_in
  # rownames(x) <- rownames_in
  x
}



#' @export
pmax <- function(..., na.rm = FALSE) {
  UseMethod("pmax")
}



#' @export
pmax.default <- function(..., na.rm = FALSE) {
  base::pmax(..., na.rm = na.rm)
}



#' @examples
#' pmax(
#'   parttime(c("2019", "2018", "2019-02", "2018",    "2010")),
#'   parttime(c("2020", NA,     "2019-03", "2018-01", "2010")))
#'
#' @export
pmax.partial_time <- function(..., na.rm = FALSE) {
  pmin_pmax_handler(base::max, ..., na.rm = na.rm)
}



#' @export
pmin <- function(..., na.rm = FALSE) {
  UseMethod("pmin")
}



#' @export
pmin.default <- function(..., na.rm = FALSE) {
  base::pmin(..., na.rm = na.rm)
}



#' @export
pmin.partial_time <- function(..., na.rm = FALSE) {
  pmin_pmax_handler(base::min, ..., na.rm = na.rm)
}

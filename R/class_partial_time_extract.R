#' Indexing operators for partial_time objects
#'
#' Indexing operators repurpose matrix indexing for indexing into parttime
#' fields. When only \code{i} is provided, the \code{parttime} vector is sliced.
#' Whenever \code{j} is provided, the individual fields are indexed out of an
#' internal matrix.
#'
#' @param x an object from which to extract element(s).
#' @param i row indicies specifying element(s) to extract or replace. For
#'   further details, see \link[base]{Extract}.
#' @param j column indicies specifying element(s) to extract or replace. For
#'   further details, see \link[base]{Extract}.
#' @inheritParams base::`[`
#' @param ... arguments unused
#'
#' @examples
#' x <- as.parttime(c("2019", "2019-02", "2019-02-02"))
#' # <partial_time<YMDhms+tz>[3]>
#' # [1] "2019"       "2019-02"    "2019-02-02"
#'
#' x[, c(1, 3)]
#' #            year day
#' # 2019       2019  NA
#' # 2019-02    2019  NA
#' # 2019-02-02 2019   2
#'
#' x[, "month"]
#' #       2019    2019-02 2019-02-02
#' #         NA          2          2
#'
#' x[, "month", drop = FALSE]
#' #            month
#' # 2019          NA
#' # 2019-02        2
#' # 2019-02-02     2
#'
#' @name parttime_extract
#' @rdname parttime_extract
#' @export
`[.partial_time` <- function(x, i, j, ...) {
  # handle case where field(s) are assigned to directly
  if (!missing(j)) {
    if (missing(i)) i <- TRUE
    return(vctrs::field(x, "pttm_mat")[i, j, ...])
  }

  # otherwise, use vctrs indexing
  NextMethod()
}



#' @rdname parttime_extract
#' @export
`[[.partial_time` <- function(x, i, j, ..., value) {
  if (!missing(j)) {
    return(vctrs::field(x, "pttm_mat")[[i, j, ...]])
  }

  # otherwise, use vctrs indexing
  NextMethod()
}



#' Subsetting assignment of partial_time objects
#'
#' @param x an object from which to extract element(s) or in which to replace
#'   element(s).
#' @param i indicies specifying elements to extract or replace. For further
#'   details, see \link[base]{Extract}.
#' @param reflow a \code{logical} indicating whether modified data fields should
#'   be reflowed, cascading range overflow. Setting to \code{FALSE} permits
#'   invalid dates, but saves on compute. Generally, it should only be disabled
#'   when multiple calculations are performed back-to-back and the dates only
#'   need to be reflowed once at the end of the calculation.
#' @inheritParams base::`[<-`
#' @param ... arguments unused
#'
#' @examples
#' x <- as.parttime(c("2019", "2019-02", "2019-02-02"))
#' # <partial_time<YMDhms+tz>[3]>
#' # [1] "2019"       "2019-02"    "2019-02-02"
#'
#' x[c(1, 3)] <- as.parttime(c("2000", "1999"))
#' # <partial_time<YMDhms+tz>[3]>
#' # [1] "2000"    "2019-02" "1999"
#'
#' x[,"month"] <- 3
#' # <partial_time<YMDhms+tz>[3]>
#' # [1] "2000-03" "2019-03" "1999-03"
#'
#' @rdname parttime_extract
#' @export
`[<-.partial_time` <- function(x, i, j, ..., reflow = TRUE, value) {
  # handle case where field(s) are assigned to directly
  if (!missing(j)) {
    if (missing(i)) i <- TRUE
    vctrs::field(x, "pttm_mat")[i, j, ...] <- value
    if (reflow) {
      vctrs::field(x, "pttm_mat")[i,] <- reflow_fields(vctrs::field(x, "pttm_mat")[i, , drop = FALSE])
    }
    return(x)
  }

  # handle empty case
  if (is.numeric(i) && !length(i)) return(x)
  else if (is.logical(i) && !any(i)) return(x)

  value <- vctrs::vec_recycle(as.parttime(value), size = length(x[i]))
  vctrs::field(x, "pttm_mat")[i, ] <- vctrs::field(value, "pttm_mat")

  # clean up rownames
  if (is.null(rownames(vctrs::field(x, "pttm_mat"))))
    rownames(vctrs::field(x, "pttm_mat")) <- rep(NA_character_, length(x))

  rownames(vctrs::field(x, "pttm_mat"))[i] <- rownames(vctrs::field(value, "pttm_mat"))

  x
}



#' @rdname parttime_extract
#' @export
`[[<-.partial_time` <- function(x, i, ..., value) {
  x[i, ...] <- value
  x
}

#' @importFrom methods setClass
methods::setClass("timespan")



#' Create a partial timespan object
#'
#' @details
#' Partial timespans are vector representations of an array of (possibly
#' missing) datetime fields. They represent timespans while accounting for the
#' possibility that their start and end might not be fully known. The start and
#' end are represented similarly to `partial_time` objects, and represent a
#' lower and upper bound for the timespan, and may be either inclusive or
#' exclusive.
#'
#' Internally, `partial_timespan` objects are represented as a three-dimensional
#' array of partial time fields, with an added column representing whether each
#' time is inclusive or exclusive. You may inspect this representation using
#' `vctrs::field(<tmspn>, "tmspn_arr")`.
#'
#' @param start vector of datetime objects to start timespans
#' @param end vector of datetime objects to end timespans
#' @param inclusive vector or matrix of logicals where each row is composed of
#'   two logical values indicating whether the timespan start and end are
#'   inclusive respectively
#'
#' @return A `partial_timespan` object. See Details for further information.
#'
#' @export
timespan <- function(start, end, inclusive = c(TRUE, FALSE)) {
  common_size <- vctrs::vec_size_common(start, end)

  inclusive <- t(matrix(
    rep_len(inclusive, common_size * 2),
    nrow = 2)
  )

  start <- as.parttime(start)
  start <- vctrs::vec_recycle(start, common_size)

  end <- as.parttime(end)
  end <- vctrs::vec_recycle(end, common_size)

  xdim <- dim(vctrs::field(start, "pttm_mat"))
  xdimnames <- dimnames(vctrs::field(start, "pttm_mat"))

  as.timespan(
    array(
      c(
        vctrs::field(start, "pttm_mat"),
        inclusive[, 1],
        vctrs::field(end, "pttm_mat"),
        inclusive[, 2]
      ),
      dim = c(xdim[[1]], xdim[[2]] + 1, 2),
      dimnames = list(
        xdimnames[[1]],
        c(xdimnames[[2]], "inclusive"),
        c("lb", "ub")
      )
    )
  )
}

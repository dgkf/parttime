#' Rank a partial_time for base sorting and ordering
#'
#' `base::sort()` and `base::order()` reach a classed vector through
#' [base::xtfrm()].  Without a method they do not fall back to anything
#' meaningful for a record type: `order()` returns the identity permutation and
#' `sort()` returns an empty vector, both without a warning.  This makes them
#' agree with [vctrs::vec_order()], which ranks by component from the year down.
#'
#' A wholly missing value ranks `NA`, so `sort()` drops it as it does for any
#' other vector.
#'
#' @param x a partial_time object
#'
#' @return An integer vector of ranks.
#'
#' @examples
#' x <- as.parttime(c("2015-04-13", "2015", "2015-04", "2014-12-31"))
#' order(x)
#' format(sort(x))
#'
#' @export
xtfrm.partial_time <- function(x) {
  ranks <- order(vctrs::vec_order(x))
  ranks[is.na(x)] <- NA_integer_
  ranks
}

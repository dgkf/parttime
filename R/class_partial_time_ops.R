# methods  getGroupMembers("Arith")
# methods  getGroupMembers("Compare")
# methods  getGroupMembers("Logic")

compare_matrices <- function(x1, x2) {
  # greater: 1, equal: 0, less than: -1
  (x1 > x2) * 2 - 1 + (x1 == x2)
}



compare_timespans <- function(x1, x2) {
  # determine index of "inclusive" field
  inc <- .i(x1, 2, "inclusive")
  
  # flags for records where timespan intersection comparison is inclusive
  inc1 <- x1[,inc,1] & x2[,inc,2] 
  inc2 <- x1[,inc,2] & x2[,inc,1] 
  
  # integer array with dimensions omitting ub and 'inclusive' field
  a <- b <- array(0L, dim = dim(x1)[1:2] - c(0, 1))
  
  # take a memory footprint hit for the sake of keeping our sanity in development
  x1_lb <- extract(x1,,-inc,1,drop = 3)
  x1_ub <- extract(x1,,-inc,2,drop = 3)
  x2_lb <- extract(x2,,-inc,1,drop = 3)
  x2_ub <- extract(x2,,-inc,2,drop = 3)
 
  a <- compare_matrices(x1_lb, x2_ub)
  a[!inc1 & col(a) == apply(a, 1, Position, f = Negate(is.na), right = TRUE) & a == 0] <- 1
  a[col(a) > apply(a != 0, 1, Position, f = isTRUE)] <- 0
  a <- t(apply(a, 1, cumsum))
  
  b <- compare_matrices(x1_ub, x2_lb)
  b[!inc2 & col(b) == apply(b, 1, Position, f = Negate(is.na), right = TRUE) & b == 0] <- -1
  b[col(b) > apply(b != 0, 1, Position, f = isTRUE)] <- 0
  b <- t(apply(b, 1, cumsum))
  
  out <- array(NA_integer_, dim = dim(a))
  out <- (a > 0) - (b < 0)
  out[out == 0 | (a > 0 & b < 0)] <- NA
  out
}




gt_lt_gte_lte_timespans <- function(generic, e1, e2) {
  drop_cols <- .i(vctrs::field(e1, "tmspn_arr"), 2, "tzhour", "tzmin")
  na <- ifelse(is.na(e1) | is.na(e2), NA_integer_, 0)
  
  e1 <- extract(vctrs::field(to_gmt(e1), "tmspn_arr"),,-drop_cols,, drop = FALSE)
  e2 <- extract(vctrs::field(to_gmt(e2), "tmspn_arr"),,-drop_cols,, drop = FALSE)
  
  # build comparison matrix
  x <- cbind(na, compare_timespans(e1, e2))
  
  # apply comparison
  x <- do.call(generic, list(x, 0))
  parttime_logical(x)
}



#' Not-equal comparison handler for partial_time objects
#' 
#' @param generic the generic operator selected
#' @param e1 objects
#' @param e2 objects
#'
#' @examples
#' x <- as.parttime(c("2019", "2018-01",    NA, "2011"))
#' y <- as.parttime(c("2019", "2018-01-03", NA, "2010-01"))
#'
#' x != y
#'
neq_parttimes <- function(generic, e1, e2) {
  x <- vctrs::field(e1, "pttm_mat") != vctrs::field(e2, "pttm_mat")
  x <- t(apply(x, 1, cumsum)) > 0
  x_any <- which(apply(x, 1, any))
  x[x_any,] <- x[x_any,] | is.na(x[x_any,])
  parttime_logical(x)
}



#' Equal comparison handler for partial_time objects
#' 
#' @param generic the generic operator selected
#' @param e1 objects
#' @param e2 objects
#' 
#' @examples
#' x <- as.parttime(c("2019", "2018-01-04", NA, "2011"))
#' y <- as.parttime(c("2019", "2018-01-03", NA, "2010-01"))
#'
#' x == y
#'
eq_parttimes <- function(generic, e1, e2) {
  x <- vctrs::field(e1, "pttm_mat") == vctrs::field(e2, "pttm_mat")
  x_nall <- which(apply(!x, 1, any))
  x[x_nall,] <- !(!x[x_nall,] | is.na(x[x_nall,]))
  parttime_logical(x)
}



#' Handler for Ops generics for partial_time objects
#' 
#' @param e1 objects
#' @param e2 objects
#'
#' @examples
#'                                        # when assume_tz "GMT"         when assume_tz NA
#'                                        # ---------------------------  ---------------------------
#'                                        # raw    possibly  definitely   raw   possibly  definitely  
#'                                        # -----  --------- -----------  ----- --------- -----------
#' parttime(1998) < parttime(1999)        # TRUE   TRUE      TRUE         NA    TRUE      FALSE
#' parttime(1998) < parttime(1997)        # FALSE  FALSE     FALSE        NA    TRUE      FALSE
#' parttime(1999) < parttime(1999)        # NA     TRUE      FALSE        NA    TRUE      FALSE
#' parttime(1998) < parttime(1999, 1, 3)  # TRUE   TRUE      TRUE         TRUE  TRUE      TRUE
#' 
#' @export
Ops.partial_time <- function(e1, e2) {
  f <- switch(.Generic,
    "==" = eq_parttimes,
    "!=" = neq_parttimes,
    NULL)
  
  if (!is.null(f)) return(f(.Generic, e1, e2))
  do.call(.Generic, list(as.timespan(e1), as.timespan(e2)))
}



#' Handler for Ops generics for timespan objects
#' 
#' @param e1 objects
#' @param e2 objects
#'
#' @export
Ops.timespan <- function(e1, e2) {
  f <- switch(.Generic,
    ">"  = gt_lt_gte_lte_timespans,
    "<"  = gt_lt_gte_lte_timespans,
    ">=" = gt_lt_gte_lte_timespans,
    "<=" = gt_lt_gte_lte_timespans,
    # "==" = eq_timespans,
    # "!=" = neq_timespans,
    NULL)
  
  if (is.null(f)) 
    warning(sprintf("'%s' not defined for \"timespan\" objects", .Generic))

  f(.Generic, complete_timespan(e1), complete_timespan(e2))
}
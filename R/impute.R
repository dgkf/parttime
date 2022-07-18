#' Impute a partial time object with a timestamp or specific fields
#'
#' @param x a datetime-like object to impute
#' @param time a datetime-like object to use for imputation
#' @param tz a character timezone name for imputation, a character value to use
#'   as the timezone part of the datetime or an numeric minute offset.
#' @param ... additional individual named fields to impute. Can be one of
#'   "year", "month", "day", "hour", "min", "sec", "secfrac", "tzhour", "tzmin".
#'
#' @return a new partial_time with specified fields imputed
#'
#' @rdname impute_time
#'
#' @export
impute_time <- function(x, time, tz, ...) {
  UseMethod("impute_time")
}



#' @rdname impute_time
#' @export
impute_date <- function(x, time, ..., res = "day") {
  impute_time(x, time = time, ..., res = res)
}




#' @rdname impute_time
#' @export
impute_time_min <- function(x, tz = "-1200", ...) {
  impute_time(x, time = time_min(), tz = tz, ...)
}



#' @rdname impute_time
#' @export
impute_date_min <- function(x, ..., res = "day") {
  impute_time_min(x, ..., res = res)
}



#' @rdname impute_time
#' @export
impute_time_max <- function(x, tz = "+1400", ...) {
  impute_time(x, time = time_max(), tz = tz, ...)
}



#' @rdname impute_time
#' @export
impute_date_max <- function(x, ..., res = "day") {
  impute_time_max(x, ..., res = res)
}



#' @rdname impute_time
#' @export
impute_time_mid <- function(x, tz = "GMT", ...) {
  impute_time(x, time = time_mid(), tz = tz, ...)
}



#' @rdname impute_time
#' @export
impute_date_mid <- function(x, ..., res = "day") {
  impute_time_mid(x, ..., res = res)
}



#' @rdname impute_time
#' @export
impute_time.default <- function(x, time, tz = "GMT", ...) {
  impute_time(as.parttime(x), time, tz = tz, ...)
}



#' @rdname impute_time
#' @export
impute_time.POSIXt <- function(x, time, tz = "GMT", ...) {
  impute_time(as.parttime(x), time, tz = tz, ...)
}



#' @param res the highest resolution datetime field used for imputation. Either
#'   a character value represented the highest resolution field or \code{NULL}
#'   to impute all fields. For the \code{impute_date} family of functions,
#'   defaults to \code{"day"}, or \code{NULL} otherwise.
#'
#' @rdname impute_time
#' @export
impute_time.partial_time <- function(x, time, tz = "GMT", ..., res = NULL) {
  dots <- list(...)
  tz <- interpret_tz(tz)

  if (missing(time)) {
    impute_pttm <- parttime(NA)
    impute_dots <- dots[names(dots) %in% colnames(vctrs::field(impute_pttm, "pttm_mat"))]

    # trigger error for missing time if dots don't include imputation fields
    if (!length(impute_dots)) time
    impute_dots <- do.call(vctrs::vec_recycle_common, impute_dots)
    impute_pttm <- vctrs::vec_recycle(impute_pttm, length(impute_dots[[1]]))

    # fill out new imputations with input
    for (i in names(impute_dots)) {
      vctrs::field(impute_pttm, "pttm_mat")[, i] <- impute_dots[[i]]
    }

  } else if ("partial_time" %in% class(time)) {
    impute_pttm <- time
  } else {
    impute_pttm <- as.parttime(as.character(time))
  }

  if (!is.null(res)) {
    fields <- seq_len(match(res, datetime_parts, nomatch = length(datetime_parts)))
    vctrs::field(impute_pttm, "pttm_mat")[, -fields] <- NA_integer_
  }

  tzhour_na <- is.na(vctrs::field(impute_pttm, "pttm_mat")[, "tzhour"])
  vctrs::field(impute_pttm, "pttm_mat")[tzhour_na, "tzhour"] <- tz %/% 60

  tzmin_na <- is.na(vctrs::field(impute_pttm, "pttm_mat")[, "tzmin"])
  vctrs::field(impute_pttm, "pttm_mat")[tzmin_na, "tzmin"] <- tz %% 60

  # recycle imputed partial_time to length of x
  impute_pttm <- vctrs::vec_recycle(impute_pttm, length(x))

  # fill in imputed fields, retaining entirely NA values
  x_na <- is.na(x)
  i_na <- is.na(vctrs::field(x, "pttm_mat"))
  vctrs::field(x, "pttm_mat")[i_na] <- vctrs::field(impute_pttm, "pttm_mat")[i_na]
  x[x_na] <- NA

  # normalize improper days back to month max
  x <- normalize_month_day(x)

  # propagate uncertainty back into imputed fields where necessary
  x <- propagate_na(x)

  x
}



#' @export
impute_time.matrix <- function(x, time, tz = "GMT", ...) {
  tz <- interpret_tz(tz)
  if (is.character(time)) time <- as.parttime(time)

  time <- as.matrix(time)
  time <- time[, datetime_parts, drop = FALSE]

  time[is.na(time[, "tzhour"]), "tzhour"] <- tz %/% 60
  time[is.na(time[, "tzmin"]), "tzmin"] <- tz %% 60

  xna <- is.na(x[,datetime_parts])
  x[, datetime_parts][xna] <- matrix(rep(time, nrow(x)), ncol = ncol(time), byrow = TRUE)[xna]

  x
}



impute_partial_time_to_chr <- function(x, time, ...) {
  if (!"partial_time" %in% class(x)) x <- as.parttime(x)

  if (!missing(time)) {
    if (is.character(time))
      time <- parse_iso8601(time)
    if (any(is.na(time)))
      stop("time parameter with must specify a complete timestamp.")
  }

  fields <- rbind(
    attr(x, "field"),
    if (!missing(time)) time,
    attr(x, "impute"),
    parse_iso8601("0000-01-01T01:00:00.000Z")
  )

  fields <- as.list(apply(fields, 2, Find, f = Negate(is.na)))

  with(fields, sprintf(
    "%04.f-%02.f-%02.f %02.f:%02.f:%02.f.%03.f +%02.f%02.f",
    year, month, day, hour, min, sec, secfrac * 1000, tzhour, tzmin))
}



interpret_tz <- function(tz) {
  if (!is.character(tz)) return(tz)
  if (is.na(suppressWarnings(as.numeric(tz)))) return(gmtoff(tz))
  tz <- as.numeric(tz)
  ((abs(tz) %/% 100) * 60 + abs(tz) %% 100) * sign(tz)
}

#' Coerce an object to a parttime object
#'
#' @param x an object for coersion
#'
#' @export
as.parttime <- function(x) {
  # spoof a parttime class object for dispatch to prevent recursion since
  # parttime()  function uses as.parttime.matrix
  vec_cast.partial_time(x, structure(0L, class = "partial_time"))
}



#' Cast to partial time object
#'
#' @inheritParams vctrs::vec_cast
#' @exportS3Method vec_cast partial_time
vec_cast.partial_time <- function(x, to, ...) {
  if (is.partial_time(x)) return(x)
  UseMethod("vec_cast.partial_time")
}



#' Default handler for casting to a partial time
#'
#' @inheritParams vctrs::vec_cast
#'
#' @importFrom vctrs stop_incompatible_cast
#' @exportS3Method vec_cast.partial_time default
vec_cast.partial_time.default <- function(x, to, ...) {
  if (!all(is.na(x) | is.null(x))) vctrs::stop_incompatible_cast(x, to)
  vctrs::vec_recycle(parttime(NA), size = length(x))
}



#' Coerce character date representations to parttime objects
#'
#' @inheritParams vctrs::vec_cast
#'
#' @examples
#' dates <- c(
#'   NA,
#'   "2001",
#'   "2002-01-01",
#'   "2004-245", # yearday
#'   "2005-W13",  # yearweek
#'   "2006-W02-5",  # yearweek + weekday
#'   "2007-10-01T08",
#'   "2008-09-20T08:35",
#'   "2009-08-12T08:35.048",  # fractional minute
#'   "2010-07-22T08:35:32",
#'   "2011-06-13T08:35:32.123",  # fractional second
#'   "2012-05-23T08:35:32.123Z",  # Zulu time
#'   "2013-04-14T08:35:32.123+05",  # time offset from GMT
#'   "2014-03-24T08:35:32.123+05:30",  # time offset with min from GMT
#'   "20150101T08:35:32.123+05:30")  # condensed form
#'
#' as.parttime(dates)
#'
#' \dontrun{
#' ### vctrs experiments informing design of parttime ###
#'
#' # using a rcrd (record) style vector
#' rcrd_test <- vctrs::new_rcrd(
#'   fields = list(a = 1:3, b = 4:6),
#'   class = numeric())
#'
#' tibble(x = 1:3, y = rcrd_test)
#' # okay
#'
#' tibble(x = 1:3) %>% mutate(y = rcrd_test)
#' # Error: Column `y` must be length 3 (the number of rows) or one, not 2
#'
#' tibble(x = 1:3) %>% { .$y <- rcrd_test; . }
#' # okay (stand-in for mutate until dplyr v0.9.0)
#'
#' }
#'
#' @exportS3Method vec_cast.partial_time character
vec_cast.partial_time.character <- function(x, to, ...) {
  pttm_mat <- if (length(x)) match_iso8601_to_matrix(x)
    else match_iso8601_to_matrix("")[0, , drop = FALSE]

  tzhour_na <- is.na(pttm_mat[, "tzhour"])
  all_na <- apply(is.na(pttm_mat), 1, all)

  if (any(tzhour_na)) {
    gmt_offset <- interpret_tz(getOption("parttime.assume_tz_offset", NA))
    pttm_mat[!all_na & tzhour_na, "tzhour"] <- gmt_offset %/% 60
    pttm_mat[!all_na & tzhour_na, "tzmin"]  <- gmt_offset %%  60
  }

  # when tzhour is available
  pttm_mat[!tzhour_na & is.na(pttm_mat[, "tzmin"]), "tzmin"] <- 0

  as.parttime(pttm_mat)
}



#' Cast a matrix to a partial time
#'
#' @inheritParams vctrs::vec_cast
#'
#' @exportS3Method vec_cast.partial_time matrix
vec_cast.partial_time.matrix <- function(x, to, ...) {
  stopifnot(ncol(x) == 9)
  stopifnot(all(datetime_parts %in% colnames(x)))

  vctrs::new_rcrd(
    fields = list(pttm_mat = x),
    class = "partial_time"
  )
}



#' Cast partial time to logical
#'
#' @inheritParams vctrs::vec_cast
#'
#' @importFrom vctrs vec_cast.logical
#' @exportS3Method vec_cast.logical partial_time
vec_cast.logical.partial_time <- function(x, to, ...) {
  unname(is.na(x))
}



coerce_parital_time_to_POSIXlt <- function(x, tz = "GMT", ...,  warn = TRUE) {
  if (warn) warn_partial(x)

  strptime(
    sprintf(
      "%04.f-%02.f-%02.fT%02.f:%02.f:%02.f.%s+%02.f%02.f",
      attr(x, "fields")[, "year"]    %|NA|% 0,
      attr(x, "fields")[, "month"]   %|NA|% 0,
      attr(x, "fields")[, "day"]     %|NA|% 0,
      attr(x, "fields")[, "hour"]    %|NA|% 0,
      attr(x, "fields")[, "min"]     %|NA|% 0,
      attr(x, "fields")[, "sec"]     %|NA|% 0,
      substring(sprintf("%.03f", attr(x, "fields")[, "secfrac"] %|NA|% 0), 3),
      attr(x, "fields")[, "tzhour"]  %|NA|% 0,
      abs(attr(x, "fields")[, "tzmin"] %|NA|% 0)),
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = tz,  # sets origin for tz offset - assumes "GMT" as per iso8601
    ...)
}



#' @export
as.character.partial_time <- function(x, ...) {
  xf <- attr(x, "fields")
  paste0(
    ifelse(is.na(xf[, "year"]),     "", sprintf("%04d",  xf[, "year"])),
    ifelse(is.na(xf[, "month"]),    "", sprintf("-%02d", xf[, "month"])),
    ifelse(is.na(xf[, "day"]),      "", sprintf("-%02d", xf[, "day"])),
    ifelse(is.na(xf[, "hour"]),     "", sprintf(" %02d", xf[, "hour"])),
    ifelse(is.na(xf[, "min"]),      "", sprintf(":%02d", xf[, "min"])),
    ifelse(is.na(xf[, "sec"]),      "", sprintf(":%02d", xf[, "sec"])),
    ifelse(is.na(xf[, "secfrac"]),  "", substring(sprintf("%.03f", xf[, "secfrac"]), 3)),
    ifelse(is.na(xf[, "tzhour"]),   "", sprintf(" %02d", xf[, "tzhour"])),
    ifelse(is.na(xf[, "tzmin"]),    "", sprintf("%02d", abs(xf[, "tzmin"]))))
}



#' @export
as.data.frame.partial_time <- function(x, ...) {
  as.data.frame(vctrs::field(x, "pttm_mat"))
}



#' @export
as.matrix.partial_time <- function(x, ...) {
  vctrs::field(x, "pttm_mat")
}



#' @export
as.POSIXlt.partial_time <- function(x, ..., warn = TRUE) {
  if (warn) warn_partial(x)
  coerce_parital_time_to_POSIXlt(x, ..., warn = FALSE)
}



#' @export
as.Date.partial_time <- function(x, tz = "GMT", ..., warn = TRUE) {
  if (warn) warn_partial(x, "year", "month", "day")
  as.Date(as.POSIXlt(x, tz = tz, ..., warn = FALSE))
}



#' @export
as.double.partial_time <- function(x, ..., warn = TRUE) {
  if (warn) warn_partial(x, "year", "month", "day", "hour", "min", "sec")
  as.numeric(as.POSIXlt(x, ..., warn = FALSE))
}



#' @export
as.POSIXct.partial_time <- function(x, ..., warn = TRUE) {
  if (warn) warn_partial(x, "year", "month", "day", "hour", "min", "sec")
  as.POSIXct(as.POSIXlt(x, ..., warn = FALSE))
}

#' Coerce an object to a parttime object
#'
#' @param x an object for coersion
#' @param format a `function` or `character` value. If a
#'   `function`, it should accept a character vector and return a matrix of
#'   parttime components. If a `character` it should provide a regular
#'   exprssion which contains capture groups for each of the parttime
#'   components.  See [parse_to_parttime_matrix]'s `regex` parameter
#'   for more details.
#' @param on.na a `function` used to signal a condition for new `NA` values
#'   introduced by coercion, a `character` value among `"error"`, `"warning"` or
#'   `"suppress"` (for silencing messages) or `NULL` equivalent to `"suppress"`.
#' @param ... Additional arguments passed to `format` when a function is
#'   provided.
#'
#' @return `parttime` vector. See the Details section of \link{parttime} for
#'   further information.
#'
#' @examples
#' as.parttime(c("1985-10-18", "1991-08-23", "1996-09-26"))
#' # <partial_time<YMDhmsZ>[3]>
#' # [1] "1985-10-18" "1991-08-23" "1996-09-26"
#'
#' as.parttime(c("1234", "5678"), format = "(?<year>\\d{4})")
#' # <partial_time<YMDhmsZ>[2]>
#' # [1] "1234" "5678"
#'
#' # format function that returns a matrix of components
#' utf8_str <- function(x) intToUtf8(utf8ToInt(x) - 16)
#' as.parttime(c("B@", "B@A@"), format = function(x) cbind(year = sapply(x, utf8_str)))
#' # <partial_time<YMDhmsZ>[2]>
#' # [1] "2000" "2010"
#'
#' # format function that returns a parttime object by first pre-processing input
#' as.parttime("B@BB", format = function(x) as.parttime(utf8_str(x)))
#' # <partial_time<YMDhmsZ>[1]>
#' # [1] "2022"
#'
#' # format function that returns a parttime object by manual construction
#' as.parttime("AIII", format = function(x) parttime(year = as.numeric(utf8_str(x))))
#' # <partial_time<YMDhmsZ>[1]>
#' # [1] "1999"
#'
#' @export
as.parttime <- function(x, ..., format = parse_iso8601_datetime, on.na = "warning") {
  # spoof a parttime class object for dispatch to prevent recursion since
  # parttime()  function uses as.parttime.matrix
  pttm <- structure(0L, class = "partial_time")
  vec_cast.partial_time(x, pttm, ..., format = format, on.na = on.na)
}



#' Cast to partial time object
#'
#' @inheritParams vctrs::vec_cast
#' @return A `partial_time` vector
#'
#' @importFrom vctrs vec_cast
#' @method vec_cast partial_time
#' @exportS3Method vec_cast partial_time
vec_cast.partial_time <- function(x, to, ...) {
  if (is.partial_time(x)) {
    return(x)
  }
  UseMethod("vec_cast.partial_time")
}



#' Default handler for casting to a partial time
#'
#' @inheritParams vctrs::vec_cast
#'
#' @return A `partial_time` vector
#'
#' @importFrom vctrs stop_incompatible_cast
#' @exportS3Method vec_cast.partial_time default
vec_cast.partial_time.default <- function(x, to, ...) {
  if (!all(is.na(x) | is.null(x))) {
    vctrs::stop_incompatible_cast(x, to, x_arg = "x", to_arg = "to")
  }
  vctrs::vec_recycle(parttime(NA), size = length(x))
}



#' Cast a Date to a partial time
#'
#' A `Date` carries a year, month and day and no time of day, so the time
#' components are missing rather than zero.  It names a calendar date rather
#' than an instant, so it carries no UTC offset either and takes the assumed
#' one, which makes `as.parttime(as.Date("2001-01-01"))` and
#' `as.parttime("2001-01-01")` the same value.
#'
#' A partial date does not survive a `Date`: `"2001-01"` names a month, but
#' reading it gives the first day of that month, a day nobody collected.  The
#' `missing_*` arguments mark such components per element, so a vector holding a
#' mixture of precisions can be restored to the partial values it came from.
#'
#' @inheritParams vctrs::vec_cast
#' @param missing_year,missing_month,missing_day a logical vector, recycled to
#'   the length of `x`, marking the elements whose component was never
#'   collected.  A `Date` carries no time of day and names no instant, so the
#'   time components and the UTC offset are missing already and take no
#'   argument.
#'
#' @return A `partial_time` vector
#'
#' @examples
#' as.parttime(as.Date(c("2001-01-01", NA)))
#'
#' # the second value was collected to the month, so its day was never observed
#' as.parttime(
#'   as.Date(c("2001-01-15", "2001-02-01")),
#'   missing_day = c(FALSE, TRUE)
#' )
#'
#' @exportS3Method vec_cast.partial_time Date
vec_cast.partial_time.Date <- function(
    x, to, ...,
    missing_year = FALSE, missing_month = FALSE, missing_day = FALSE) {
  lt <- as.POSIXlt(x, tz = "UTC")
  assumed <- interpret_tz(getOption("parttime.assume_tz_offset", NA)) / 60
  size <- length(x)
  parttime(
    year = drop_component(lt$year + 1900L, missing_year, size, "missing_year"),
    month = drop_component(lt$mon + 1L, missing_month, size, "missing_month"),
    day = drop_component(lt$mday, missing_day, size, "missing_day"),
    # A missing date takes no assumed offset.  That would leave a value with no
    # date but an offset, which `is.na()` reads as present, where the character
    # cast gives `NA` throughout.
    tzhour = ifelse(is.na(lt$year), NA_real_, assumed)
  )
}



#' Drop the components an input records but never collected
#'
#' @param observed the component as read from the input
#' @param drop a logical vector, recycled to `size`, marking the elements whose
#'   component is to be treated as never collected
#' @param size the length of the value being cast
#' @param arg the name of the argument `drop` came from, for error messages
#' @return `observed`, with the marked elements set to `NA`
#' @noRd
drop_component <- function(observed, drop, size, arg) {
  drop <- vctrs::vec_cast(drop, logical(), x_arg = arg)
  drop <- vctrs::vec_recycle(drop, size, x_arg = arg)
  if (anyNA(drop)) {
    stop("`", arg, "` marks which elements were never collected, so it cannot ",
         "itself be `NA`.", call. = FALSE)
  }
  observed[drop] <- NA
  observed
}



#' Cast a date-time to a partial time
#'
#' A `POSIXct` carries every component, including the UTC offset, which is read
#' from the value rather than assumed.  The offset is the one in force at that
#' instant, so a zone observing daylight saving gives different offsets either
#' side of the change.
#'
#' Reading a value collected to a coarser precision fills the components below
#' it, so a day-precision observation arrives at midnight in some zone.  Every
#' component, the offset included, can be marked per element, so a column
#' holding a mixture of precisions can be restored to the partial values it came
#' from.
#'
#' @inheritParams vctrs::vec_cast
#' @param missing_year,missing_month,missing_day,missing_hour,missing_minute,missing_second,missing_tz
#'   a logical vector, recycled to the length of `x`, marking the elements whose
#'   component was never collected.
#'
#' @return A `partial_time` vector
#'
#' @examples
#' as.parttime(as.POSIXct("2001-06-15 10:30:15", tz = "UTC"))
#'
#' # a value collected only to the day, whose time of day is an artefact
#' as.parttime(
#'   as.POSIXct("2001-06-15 00:00:00", tz = "UTC"),
#'   missing_hour = TRUE, missing_minute = TRUE, missing_second = TRUE,
#'   missing_tz = TRUE
#' )
#'
#' @exportS3Method vec_cast.partial_time POSIXt
vec_cast.partial_time.POSIXt <- function(
    x, to, ...,
    missing_year = FALSE, missing_month = FALSE, missing_day = FALSE,
    missing_hour = FALSE, missing_minute = FALSE, missing_second = FALSE,
    missing_tz = FALSE) {
  lt <- as.POSIXlt(x)
  size <- length(x)
  parttime(
    year = drop_component(lt$year + 1900L, missing_year, size, "missing_year"),
    month = drop_component(lt$mon + 1L, missing_month, size, "missing_month"),
    day = drop_component(lt$mday, missing_day, size, "missing_day"),
    hour = drop_component(lt$hour, missing_hour, size, "missing_hour"),
    min = drop_component(lt$min, missing_minute, size, "missing_minute"),
    sec = drop_component(lt$sec, missing_second, size, "missing_second"),
    # `%z` renders the offset in force at that instant, which
    # `as.POSIXlt()$gmtoff` would too but `?DateTimeClasses` documents as
    # optional and `NA` where the platform does not supply it.
    tzhour = drop_component(
      interpret_tz(format(x, "%z")) / 60, missing_tz, size, "missing_tz"
    )
  )
}





#' Coerce character date representations to parttime objects
#'
#' @param ... Additional arguments passed to \code{format} if a function is
#'   provided.
#' @inheritParams vctrs::vec_cast
#' @inheritParams as.parttime
#'
#' @return A `partial_time` vector
#'
#' @examples
#' dates <- c(
#'   NA,
#'   "2001",
#'   "2002-01-01",
#'   "2004-245", # yearday
#'   "2005-W13", # yearweek
#'   "2006-W02-5", # yearweek + weekday
#'   "2007-10-01T08",
#'   "2008-09-20T08:35",
#'   "2009-08-12T08:35.048", # fractional minute
#'   "2010-07-22T08:35:32",
#'   "2011-06-13T08:35:32.123", # fractional second
#'   "2012-05-23T08:35:32.123Z", # Zulu time
#'   "2013-04-14T08:35:32.123+05", # time offset from GMT
#'   "2014-03-24T08:35:32.123+05:30", # time offset with min from GMT
#'   "20150101T083532.123+0530" # condensed form
#' )
#'
#' as.parttime(dates)
#'
#' @exportS3Method vec_cast.partial_time character
vec_cast.partial_time.character <- function(
    x, to, ...,
    format = parse_iso8601_datetime, on.na = warning) {
  if (is.null(on.na)) {
    on.na <- "suppress"
  }

  if (is.character(on.na)) {
    on.na <- switch(on.na,
      warning = warning,
      error = stop,
      suppress = identity,
      stop("Invalid argument passed to `on.na`. See ?as.parttime for details.")
    )
  }

  pttm_mat <- if (length(x) > 0L) {
    if (is.function(format)) {
      format(x, ...)
    } else {
      parse_to_parttime_matrix(x, regex = format)
    }
  } else {
    # parsing function is irrelevant if input has no length, just use default
    parse_to_parttime_matrix(NA_character_)[NULL, , drop = FALSE]
  }

  pttm_mat <- clean_parsed_parttime_matrix(pttm_mat)
  res <- as.parttime(pttm_mat)

  # if NAs are introduced during coercion, emit on.na callback
  if (any(is.na(res) & !is.na(x))) {
    on.na(parse_failure_message(x, res))
  }

  res
}



#' Cast a matrix to a partial time
#'
#' @inheritParams vctrs::vec_cast
#'
#' @return A `partial_time` vector
#'
#' @exportS3Method vec_cast.partial_time matrix
vec_cast.partial_time.matrix <- function(x, to, ...) {
  stopifnot(ncol(x) == 7)
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
#' @return A `partial_time` vector
#'
#' @importFrom vctrs vec_cast.logical
#' @exportS3Method vec_cast.logical partial_time
vec_cast.logical.partial_time <- function(x, to, ...) {
  unname(is.na(x))
}



coerce_partial_time_to_POSIXlt <- function(x, tz = "GMT", ..., warn = TRUE) {
  if (warn) warn_partial(x)
  strptime(
    sprintf(
      "%04.f-%02.f-%02.fT%02.f:%02.f:%02.f.%s+%02.f%02.f",
      x[, "year"] %|NA|% 0,
      x[, "month"] %|NA|% 0,
      x[, "day"] %|NA|% 0,
      x[, "hour"] %|NA|% 0,
      x[, "min"] %|NA|% 0,
      x[, "sec"] %|NA|% 0,
      substring(sprintf("%.03f", x[, "sec"] %% 1 %|NA|% 0), 3),
      x[, "tzhour"] %/% 1 %|NA|% 0,
      x[, "tzhour"] %% 1 * 60 %|NA|% 0
    ),
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = tz, # sets origin for tz offset - assumes "GMT" as per iso8601
    ...
  )
}



#' @export
as.character.partial_time <- function(x, ...) {
  nna <- !is.na(x)
  out <- rep_len(NA_character_, length(x))

  out[nna] <- paste0(
    ifelse(is.na(x[nna, "year"]), "", sprintf("%04d", x[nna, "year"])),
    ifelse(is.na(x[nna, "month"]), "", sprintf("-%02d", x[nna, "month"])),
    ifelse(is.na(x[nna, "day"]), "", sprintf("-%02d", x[nna, "day"])),
    ifelse(is.na(x[nna, "hour"]), "", sprintf(" %02d", x[nna, "hour"])),
    ifelse(is.na(x[nna, "min"]), "", sprintf(":%02d", x[nna, "min"])),
    ifelse(is.na(x[nna, "sec"]), "", sprintf(":%02d", x[nna, "sec"] %/% 1)),
    ifelse(is.na(x[nna, "sec"]), "", substring(sprintf("%.03f", x[nna, "sec"] %% 1), 2)),
    ifelse(is.na(x[nna, "tzhour"]), "", sprintf(" %02d", x[nna, "tzhour"] %/% 1)),
    ifelse(is.na(x[nna, "tzhour"]), "", sprintf("%02d", abs(x[nna, "tzhour"] %% 1 * 60)))
  )

  out
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
  coerce_partial_time_to_POSIXlt(x, ..., warn = FALSE)
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

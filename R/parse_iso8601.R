#' slightly modified from parsedate - added 'secfrac' capture group
#'
#' @keywords internal
#'
re_iso8601 <- paste0(
  "^\\s*",
  "(?<year>[\\+-]?\\d{4}(?!\\d{2}\\b))",
  "(?:",
    # {datesep} can be either `-` ({dash}) or ``, but must be consistent
    "(?<datesep>(?<dash>-)|)",
    "(?:(?<month>0[1-9]|1[0-2])",
      "(?:\\g{datesep}(?<day>[12]\\d|0[1-9]|3[01]))?",
    "|",
      "W(?<week>[0-4]\\d|5[0-3])(?:-?(?<weekday>[1-7]))?",
    "|",
      "(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3(?:[0-5]\\d|6[1-6]))",
    ")",
    "(?<time>[T\\s]",
      "(?:",
        "(?:",
          "(?<hour>[01]\\d|2[0-3])",
          "(?:",
            # if {dash} captured, {timesep} must be `:`, else ``
            "(?<timesep>(?(<dash>):|))",
            "(?<min>[0-5]\\d)",
          ")?",
        "|",
          "24\\g{timesep}00",
        ")",
        "(?<frac>[\\.,]\\d+(?!:))?",
      ")?",
      "(?:",
        "\\g{timesep}",
        "(?<sec>",
          "[0-5]\\d",
          "(?<secfrac>[\\.,]\\d+)?",
        ")",
      ")?",
      "(?<tz>",
        "[zZ]",
      "|",
        "(?<tzpm>[\\+-])",
        "(?<tzhour>[01]\\d|2[0-3])",
        "(?:",
          "\\g{timesep}",
          "(?<tzmin>[0-5]\\d)",
        ")?",
      ")?",
    ")?",
  ")?$"
)



#' Parse iso8601 datetime strings as parttime matrix
#'
#' @param x A \code{character} vector of iso8601 datetime strings
#' @param warn A \code{logical} indicating whether to warn when information
#'   would be loss when coercing to a \code{parttime} matrix.
#' @param ... Additional arguments unused
#'
#' @return A matrix of capture fields discovered while parsing ISO8601-style
#'   datetimes.
#'
#' @references
#' ["ISO_8601" on Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
#'
#' @rdname parse_parttime
#' @export
#'
parse_iso8601_datetime <- function(x, warn = TRUE, ...) {
  if (is.character(x)) {
    x <- parse_iso8601_matrix(x)
  }

  # warn when week is specified without weekday, leading to loss of information
  if (warn) warn_repr_data_loss(x, includes = "week", excludes = "weekday")

  # add month, day when week, weekday available
  i <- is_iso8601_weekday(x)
  x[i, c("month", "day")] <- recalc_md_from_weekday(x[i, , drop = FALSE])

  # add month, day when yearday available
  i <- is_iso8601_yearday(x)
  x[i, c("month", "day")] <- recalc_md_from_yearday(x[i, , drop = FALSE])

  # add sec, secfrac when frac (minfrac) is available
  i <- is_iso8601_minfrac(x)
  x[i, "sec"] <- recalc_sec_from_minfrac(x[i, , drop = FALSE])

  # collapse tzmin component into tzhour
  x[, "tzhour"] <- x[, "tzhour"] + (x[, "tzmin"] %|NA|% 0) / 60

  # drop iso8601-specific columns
  x[, datetime_parts, drop = FALSE]
}



#' Parse iso8601 datetime strings as timespan array
#'
#' @note A timespan array is an internal data structure used as the backend
#' representation of timespan objects. It consists of two parttime-like matrices
#' (with the addition of an "inclusive" column), one for the lower- and
#' upper-bounds of the timespan. Collectively, this amounts to a three
#' dimensional array.
#'
#' @keywords internal
#' @rdname parse_parttime
#'
parse_iso8601_datetime_as_timespan <- function(x, ...) {
  tmspn_arr <- array(
    NA_real_,
    dim = c(length(x), length(datetime_parts) + 1L, 2L),
    dimnames = list(x, c(datetime_parts, "inclusive"), c("lb", "ub"))
  )

  m <- parse_iso8601_matrix(x)

  # user parttime handler where possible, uniquely handle yearweek format
  i <- matrix_field_cond(m, includes = "week", excludes = "weekday")
  m[!i, datetime_parts] <- parse_iso8601_datetime(m[!i, , drop = FALSE])

  # impute yearweek + weekday with first day of the week for start
  m[i, datetime_parts] <- parse_iso8601_datetime(paste0(x[i], "-1"))
  tmspn_arr[, datetime_parts, "lb"] <- clean_parsed_parttime_matrix(m)
  tmspn_arr[, "inclusive", "lb"] <- 1

  # impute yearweek + weekday with last day of the week for end
  m[i, datetime_parts] <- parse_iso8601_datetime(paste0(x[i], "-7"))
  tmspn_arr[, datetime_parts, "ub"] <- minimally_increment(clean_parsed_parttime_matrix(m))
  tmspn_arr[, "inclusive", "ub"] <- 0

  tmspn_arr
}



#' Parse an iso8601 datetime to a parttime-like matrix
#'
#' @note In addition to parttime matrix fields, the returned matrix has
#' additional columns for alternative iso8601 formats such as \code{yearday},
#' \code{yearweek} and code{weakday}.
#'
#' @keywords internal
#'
parse_iso8601_matrix <- function(dates) {
  match_m <- parse_to_parttime_matrix(dates, regex = re_iso8601)

  # fix missing tzhour, tzmin when tz is available
  match_m[match_m[, "tz"] == "Z", c("tzhour", "tzmin")] <- "00"

  # apply offset sign to tzhour and tzmin
  match_m[, "tzhour"] <- ifelse(
    nchar(match_m[, "tzhour"]),
    paste0(match_m[, "tzpm"], match_m[, "tzhour"]),
    ""
  )

  match_m[, "tzmin"]  <- ifelse(
    nchar(match_m[, "tzmin"]),
    paste0(match_m[, "tzpm"], match_m[, "tzmin"]),
    ""
  )

  fields <- c("year", "month", "day", "week", "weekday", "yearday", "hour",
    "min", "frac", "sec", "tzhour", "tzmin")

  match_m <- match_m[, fields, drop = FALSE]
  storage.mode(match_m) <- "numeric"

  match_m
}



#' Inspecting and manipulating intermediate iso8601 matrices
#'
#' An "iso8601 matrix" is a matrix of the various capture groups extraced from a
#' an iso8601 datetime string. These groups represent a superset of the fields
#' used by partial time objects, including representation for less common
#' datetime formats like yeardays, yearweeks or weekdays. Because the standard
#' provides a number of different combinations of fields that represent valid
#' strings, these functions serves to provide convenience functions for testing
#' or manipulating these less canonical representations.
#'
#' @param x A \code{numeric} matrix of possible iso8601 fields
#' @param fields A \code{character} vector of fields
#'
#' @section is_iso8601_* functions:
#' Test whether rows of the matrix represent a specific form, as evident by
#' non-missing values in specific fields.
#'
#' @keywords internal
#'
#' @name parse_iso8601_helpers
#' @rdname parse_iso8601_helpers
#'
is_iso8601_form <- function(x, fields) {
  apply(!is.na(x[, fields, drop = FALSE]), 1, all)
}

#' @keywords internal
#' @rdname parse_iso8601_helpers
is_iso8601_weekday <- function(x) {
  is_iso8601_form(x, c("year", "week", "weekday"))
}

#' @keywords internal
#' @rdname parse_iso8601_helpers
is_iso8601_yearday <- function(x) {
  is_iso8601_form(x, c("year", "yearday"))
}

#' @keywords internal
#' @rdname parse_iso8601_helpers
is_iso8601_minfrac <- function(x) {
  is_iso8601_form(x, "frac")
}

#' @section recalc_* functions:
#' Calculate canonical datetime fields from alternative representations
#'
#' @keywords internal
#' @rdname parse_iso8601_helpers
#'
recalc_md_from_weekday <- function(x) {
  dates <- strptime(
    paste(x[, "year"], x[, "week"], x[, "weekday"] - 1L, sep = "-"),
    format = "%Y-%U-%w"
  )

  cbind(month = dates$mon + 1L, day = dates$mday)
}

#' @keywords internal
#' @rdname parse_iso8601_helpers
recalc_md_from_yearday <- function(x) {
  dates <- strptime(
    paste(x[, "year"], x[, "yearday"], sep = "-"),
    format = "%Y-%j"
  )

  cbind(month = dates$mon + 1L, day = dates$mday)
}

#' @keywords internal
#' @rdname parse_iso8601_helpers
recalc_sec_from_minfrac <- function(x) {
  x[, "frac"] * 60
}

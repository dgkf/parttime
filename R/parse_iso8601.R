#' slightly modified from parsedate - added 'secfrac' capture group
re_iso8601 <- paste0(
  "^\\s*",
  "(?<year>[\\+-]?\\d{4}(?!\\d{2}\\b))",
  "(?:",
    "(?<dash>-?)",
    "(?:(?<month>0[1-9]|1[0-2])",
      "(?:\\g{dash}(?<day>[12]\\d|0[1-9]|3[01]))?",
      "|W(?<week>[0-4]\\d|5[0-3])(?:-?(?<weekday>[1-7]))?",
      "|(?<yearday>00[1-9]|0[1-9]\\d|[12]\\d{2}|3(?:[0-5]\\d|6[1-6]))",
    ")",
    "(?<time>[T\\s]",
      "(?:",
        "(?:",
          "(?<hour>[01]\\d|2[0-3])",
          "(?:(?<colon>:?)(?<min>[0-5]\\d))?|24\\:?00",
        ")",
        "(?<frac>[\\.,]\\d+(?!:))?",
      ")?",
      "(?:\\g{colon}",
        "(?<sec>[0-5]\\d)",
        "(?<secfrac>[\\.,]\\d+)?",
      ")?",
      "(?<tz>",
        "[zZ]|(?<tzpm>[\\+-])",
        "(?<tzhour>[01]\\d|2[0-3])",
        ":?",
        "(?<tzmin>[0-5]\\d)?",
      ")?",
    ")?",
  ")?$"
)


#' @rdname parse_parttime
parse_iso8601 <- function(dates) {
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
    "min", "frac", "sec", "secfrac", "tzhour", "tzmin")

  match_m <- match_m[, fields, drop = FALSE]
  storage.mode(match_m) <- "numeric"

  # when year, month, day available calculate week, weekday, yearday
  i <- !apply(is.na(match_m[, c("year", "month", "day"), drop = FALSE]), 1, any)
  if (any(i)) {
    dates <- strptime(apply(match_m[i, c("year", "month", "day"), drop = FALSE], 1, paste, collapse = "-"), format = "%Y-%m-%d")
    match_m[i, "week"] <- dates$yday %/% 7 + 1
    match_m[i, "weekday"] <- dates$yday %% 7 + 1
    match_m[i, "yearday"] <- dates$yday + 1
  }

  # add month, day when week, weekday available
  i <- !apply(is.na(match_m[, c("year", "week", "weekday"), drop = FALSE]), 1, any)
  if (any(i)) {
    dates <- strptime(apply(match_m[i, c("year", "week", "weekday"), drop = FALSE], 1, paste, collapse = "-"), format = "%Y-%U-%u")
    match_m[i, "month"] <- dates$mon + 1
    match_m[i, "day"] <- dates$mday
  }

  # add month, day when yearday available
  i <- !apply(is.na(match_m[, c("year", "yearday"), drop = FALSE]), 1, any)
  if (any(i)) {
    dates <- strptime(apply(match_m[i, c("year", "yearday"), drop = FALSE], 1, paste, collapse = "-"), format = "%Y-%j")
    match_m[i, "month"] <- dates$mon + 1
    match_m[i, "day"] <- dates$mday
  }

  # fill frac (minfrac) when sec and secfrac are provided
  i <- !apply(is.na(match_m[, c("sec", "secfrac"), drop = FALSE]), 1, any)
  if (any(i)) {
    match_m[i, "frac"] <- (match_m[i, c("sec"), drop = FALSE] + match_m[i, c("secfrac"), drop = FALSE]) / 60
  }

  # fill sec and secfrac when frac (minfrac) is provided
  i <- !apply(is.na(match_m[, c("frac"), drop = FALSE]), 1, any)
  if (any(i)) {
    match_m[i, "sec"] <- (match_m[i, c("frac"), drop = FALSE] * 60) %/% 1
    match_m[i, "secfrac"] <- (match_m[i, c("frac"), drop = FALSE] * 60) %% 1
  }

  # reduce to minimum set of columns
  # order of fields should be in decreasing resolution. the order is used for
  # matrix operations when handling operator behaviors
  match_m[, datetime_parts, drop = FALSE]
}

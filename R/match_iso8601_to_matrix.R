#' A rework of parsedate::regexpr_to_df
#'
#' Provides a vertical dataframe of matches more ammeneable to a tidyverse-style
#' analysis
#'
match_iso8601_to_matrix <- function(dates) {
  match <- regexpr(parsedate:::iso_regex, as.character(dates), perl = TRUE)
  match_m <- matrix(
    substring(
      dates,
      s <- attr(match, "capture.start"),
      s + attr(match, "capture.length") - 1),
    nrow = length(dates),
    dimnames = list(dates, colnames(s)))

  # fix missing tzhour, tzmin when tz is available
  match_m[match_m[,"tz"] == "Z", c("tzhour", "tzmin")] <- "00"

  # apply offset sign to tzhour and tzmin
  match_m[,"tzhour"] <- paste0(match_m[,"tzpm"], match_m[,"tzhour"])
  match_m[,"tzmin"]  <- ifelse(nchar(match_m[,"tzmin"]), 
    paste0(match_m[,"tzpm"], match_m[,"tzmin"]), 
    "")
    
  match_m <- match_m[,c("year", "month", "day", "week", "weekday", "yearday",
    "hour", "min", "frac", "sec", "secfrac", "tzhour", "tzmin"), drop = FALSE]
  
  storage.mode(match_m) <- "numeric"
  
  # when year, month, day available calculate week, weekday, yearday
  i <- !apply(is.na(match_m[,c("year", "month", "day"), drop = FALSE]), 1, any)
  if (any(i)) {
    dates <- strptime(apply(match_m[i, c("year", "month", "day"), drop = FALSE], 1, paste, collapse = "-"), format = "%Y-%m-%d")
    match_m[i,"week"] <- dates$yday %/% 7 + 1
    match_m[i,"weekday"] <- dates$yday %% 7 + 1
    match_m[i,"yearday"] <- dates$yday + 1
  }

  # add month, day when week, weekday available
  i <- !apply(is.na(match_m[,c("year", "week", "weekday"), drop = FALSE]), 1, any)
  if (any(i)) {
    dates <- strptime(apply(match_m[i, c("year", "week", "weekday"), drop = FALSE], 1, paste, collapse = "-"), format = "%Y-%U-%u")
    match_m[i,"month"] <- dates$mon + 1
    match_m[i,"day"] <- dates$mday
  }

  # add month, day when yearday available
  i <- !apply(is.na(match_m[,c("year", "yearday"), drop = FALSE]), 1, any)
  if (any(i)) {
    dates <- strptime(apply(match_m[i, c("year", "yearday"), drop = FALSE], 1, paste, collapse = "-"), format = "%Y-%j")
    match_m[i,"month"] <- dates$mon + 1
    match_m[i,"day"] <- dates$mday
  }

  # fill frac (minfrac) when sec and secfrac are provided
  i <- !apply(is.na(match_m[,c("sec", "secfrac"), drop = FALSE]), 1, any)
  if (any(i)) {
    match_m[i,"frac"] <- (match_m[i, c("sec"),drop = FALSE] + match_m[i, c("secfrac"),drop = FALSE]) / 60
  }

  # fill sec and secfrac when frac (minfrac) is provided
  i <- !apply(is.na(match_m[,c("frac"), drop = FALSE]), 1, any)
  if (any(i)) {
    match_m[i,"sec"] <- (match_m[i, c("frac"),drop = FALSE] * 60) %/% 1
    match_m[i,"secfrac"] <- (match_m[i, c("frac"),drop = FALSE] * 60) %% 1
  }

  # reduce to minimum set of columns
  # order of fields should be in decreasing resolution. the order is used for
  # matrix operations when handling operator behaviors
  match_m[,c("year", "month", "day", "hour", "min", "sec", "secfrac", "tzhour", "tzmin"), drop = FALSE]
}

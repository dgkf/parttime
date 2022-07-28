#' Parse a character vector to create a matrix of datetime fields
#'
#' @param dates character vector of dates to parse for iso8601 components
#' @param regex A regular expression used for parsing parttime strings. Defaults
#'   to a parser for the ISO8601 standard. Should include named capture groups
#'   for each datetime field. See Details for more information.
#'
#' @details
#' A format regular expression should separate the components of a valid
#' datetime string, capturing strings which can be coerced to numeric values for
#' each of the nine datetime fields:
#'
#' `r paste(c("\\itemize{", sprintf("  \\item{\\code{%s}}", datetime_parts), "}"), collapse = "\n")`
#'
#' @rdname parse_parttime
#' @export
#'
parse_to_parttime_matrix <- function(dates, regex = re_iso8601) {
  match <- regexpr(regex, as.character(dates), perl = TRUE)
  matrix(
    substring(
      dates,
      s <- attr(match, "capture.start"),
      s + attr(match, "capture.length") - 1),
    nrow = length(dates),
    dimnames = list(dates, colnames(s))
  )
}



clean_parsed_parttime_matrix <- function(m) {
  if (is.parttime(m)) {
    return(m)
  }

  if (!all(datetime_parts %in% colnames(m))) {
    m <- complete_parsed_parttime_matrix(m)
  }

  m <- m[, datetime_parts, drop = FALSE]
  storage.mode(m) <- "numeric"

  tzhour_na <- is.na(m[, "tzhour"])
  all_na <- apply(is.na(m), 1, all)

  if (any(tzhour_na)) {
    gmt_offset <- interpret_tz(getOption("parttime.assume_tz_offset", NA))
    m[!all_na & tzhour_na, "tzhour"] <- gmt_offset %/% 60
    m[!all_na & tzhour_na, "tzmin"]  <- gmt_offset %%  60
  }

  # when tzhour is available
  m[!tzhour_na & is.na(m[, "tzmin"]), "tzmin"] <- 0
  m[, datetime_parts, drop = FALSE]
}



complete_parsed_parttime_matrix <- function(match_m) {
  # add in any missing columns if needed
  needs_cols <- setdiff(datetime_parts, colnames(match_m))
  if (length(needs_cols) > 0L) {
    extra_cols <- matrix(
      NA_character_,
      nrow = nrow(match_m),
      ncol = length(needs_cols),
      dimnames = list(c(), needs_cols)
    )

    match_m <- cbind(match_m, extra_cols)
  }

  cols <- c(datetime_parts, setdiff(colnames(match_m), datetime_parts))
  match_m[, cols, drop = FALSE]
}

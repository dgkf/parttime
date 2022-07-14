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

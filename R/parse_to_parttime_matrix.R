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
      s + attr(match, "capture.length") - 1
    ),
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



#' Format a message communicating parse failure information
#'
#' @param input The input provided to the parser
#' @param output The parttimes returned by the parser
#' @return A message communicating failure modes for NAs introduced into output
#'   that were not present in the input
#'
#' @keywords internal
#'
parse_failure_message <- function(input, output) {
  mask_fail <- is.na(output) & !is.na(input)
  styles <- sample_date_string_styles(input[mask_fail])

  n <- sum(mask_fail)
  perc <- n / length(input) * 100

  paste0(
    wrap(
      "Values could not be parsed ",
      sprintf("(%.f of %.f (%.1f%%)). ", n, length(input), perc),
      "Examples of unique failing formats:"
    ),
    "\n\n",
    wrap_vec(indent = 4L, c(
      paste0("'", head(input[mask_fail][styles], 10), "'"),
      if (length(styles) > 10) "..."
    )),
    "\n"
  )
}



#' Find unique forms of inputs
#'
#' @param x A `character` vector of datetime strings
#' @return The indices of the first instance of unique datatime formats
#'
#' @examples
#' x <- c("2022", "T02:01", "2023", "Y1970", "2021-01", "2024-12")
#' x[parttime:::sample_date_string_styles(x)]
#'
#' @keywords internal
#'
sample_date_string_styles <- function(x) {
  fingerprints <- gsub("\\d", " ", trimws(x))
  which(!duplicated(fingerprints))
}



#' Find unique forms of missingness
#'
#' @param x A `partial_time` vector
#' @return The indices of the first instance of unique forms of missingness
#'
#' @examples
#' x <- as.parttime(c("2022", "2023", "2021-01", "2024-12"))
#' x[parttime:::sample_partial_styles(x)]
#'
#' @keywords internal
#'
sample_partial_styles <- function(x) {
  weights <- 2 ^ seq(0, ncol(vctrs::field(x, "pttm_mat")) - 1)
  fingerprints <- is.na(vctrs::field(x, "pttm_mat")) %*% weights
  which(!duplicated(fingerprints))
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

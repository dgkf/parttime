#' Regular expression for CDISC-style datetime parsing
#'
#' @keywords internal
#'
re_cdisc_datetime <- paste0(
  "^\\s*",
  "(?<year>\\d{4}(?!\\d{2}\\b)|-)",
  "(?:",
    "(?<dash>-)",
    "(?:(?<month>0[1-9]|1[0-2]|-)",
      "(?:",
        "\\g{dash}",
        "(?<day>[12]\\d|0[1-9]|3[01]|-)",
      ")?",
    ")",
    "(?<time>T",
      "(?:",
        "(?:",
          "(?<hour>[01]\\d|2[0-3]|-)",
          "(?:",
            "(?<colon>:)",
            "(?<min>[0-5]\\d|-)",
          ")?|24\\:?00",
        ")",
      ")?",
      "(?:\\g{colon}",
        "(?<sec>",
          "[0-5]\\d",
          "(?<secfrac>\\.\\d+)?",
        "|",
          "-",
        ")",
      ")?",
    ")?",
  ")?$"
)

# Further considerations
# - Can dates be "missing-on-the-end"? Like 2022-07--T-
#   these are currently valid


#' Parse cdisc datetime strings as parttime matrix
#'
#' @param x A \code{character} vector of iso8601 datetime strings
#' @param warn A \code{logical} indicating whether to warn when information
#'   would be loss when coercing to a \code{parttime} matrix.
#' @param ... Additional arguments unused
#'
#' @return A matrix of capture fields discovered while parsing CDISC-style
#'   datetimes.
#'
#' @references
#'   [CDISC SDTMIG v3.4](https://www.cdisc.org/system/files/members/standard/foundational/SDTMIG_v3.4.pdf)
#'
#' @rdname parse_parttime
#' @export
#'
parse_cdisc_datetime <- function(x, warn = TRUE, ...) {
  m <- parse_to_parttime_matrix(x, regex = re_cdisc_datetime)
  m[m == "-"] <- ""
  m <- clean_parsed_parttime_matrix(m)

  first_na <- apply(is.na(m), 1L, Position, f = identity)
  last_val <- apply(!is.na(m), 1L, Position, f = identity)

  if (any(!is.na(first_na) & !is.na(last_val) & last_val > first_na)) {
    warning(
      "Missing-in-the-middle datetime handline is experimental. ",
      "Please report bugs as you encounter them with: \n\n",
      "   bug.report(package = 'parttime')",
      "\n"
    )
  }

  m
}

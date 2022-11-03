#' Format a parttime object
#'
#' @param x A `partial_time` object
#' @param ... Additional arguments passed to \link{format_field_matrix}
#' @param quote A `logical` indicating whether to add quotation marks around
#'   formatted objects
#'
#' @return A `character` vector representation of a `partial_time` vector
#'
#' @importFrom pillar style_na
#' @export
format.partial_time <- function(x, ..., quote = TRUE) {
  x_str <- vector("character", length(x))
  x_str[] <- pillar::style_na("NA")

  xna <- is.na(x)
  x_str[!xna] <- sprintf(
    if (quote) "\"%s\"" else "%s",
    format_field_matrix(vctrs::field(x[!xna], "pttm_mat"), ...))
  x_str
}


#' Format individual components of a parttime matrix
#'
#' @param x a parttime \code{matrix} to format
#' @param verbose a \code{logical} value indicating whether to include full
#'   timestamps, even if partially missing.
#' @param tz a \code{logical} value indicating whether to include timezone
#'   information. By default, will only display time zones if they are not
#'   missing and not equal to the assumed timezone option.
#'
#' @keywords internal
#'
#' @importFrom pillar style_subtle style_na
#' @importFrom crayon col_substring
format_field_matrix <- function(x,
    verbose = getOption("parttime.print_verbose", FALSE),
    tz) {

  tzcols <- .i(x, 2, "tzhour")
  tzs <- tz_consensus(x)

  x_omit <- FALSE
  if (!isTRUE(verbose)) {
    x_omit <- matrix(
      logical(),
      nrow = nrow(x),
      ncol = ncol(x),
      dimnames = dimnames(x)
    )

    if (missing(tz) || !tz) {
      x_omit[, tzcols] <- all(is.na(x[, tzcols])) | all(x[, tzcols] == 0)
    }

    x_omit[, -tzcols] <- col(x[, -tzcols, drop = FALSE]) > apply(
      x[, -tzcols, drop = FALSE],
      1L,
      Position,
      f = Negate(is.na),
      right = TRUE
    )
  }

  x_str <- matrix(
    character(),
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = dimnames(x)
  )

  # date
  x_str[, "year"] <- format_field(x[, "year"], 4)

  x_str[, "month"] <- paste0(
    pillar::style_subtle("-"),
    format_field(x[, "month"], 2, TRUE)
  )

  x_str[, "day"] <- paste0(
    pillar::style_subtle("-"),
    format_field(x[, "day"], 2, TRUE)
  )

  # time
  x_str[, "hour"] <- paste0(" ", format_field(x[, "hour"], 2, TRUE))

  x_str[, "min"] <- paste0(pillar::style_subtle(":"), format_field(x[, "min"], 2))

  x_str[, "sec"] <- paste0(
    pillar::style_subtle(":"),
    format_field(x[, "sec"] %/% 1, 2),
    ifelse(
      is.na(x[,"sec"]) | (r <- round(x[, "sec"] %% 1, 3)) == 0,
      "",
      substring(format_field(r, fmt = "%.3f"), 2)
    )
  )

  # optional timezone (timespan/duration have no tz elements)
  if ("tzhour" %in% colnames(x_str)) {
    if ((!missing(tz) && tz) || identical(tzs, FALSE)) {
      i <- !is.na(x[, "tzhour"])

      assumed_tz <- interpret_tz(getOption("parttime.assume_tz_offset", NA))
      if (missing(tz) && !is.na(assumed_tz)) {
        i <- i & (x[, "tzhour"] * 60) != assumed_tz
      } else if (!missing(tz)) {
        i <- i & tz
      }

      x_str[!i, "tzhour"] <- ""
      x_str[i, "tzhour"] <- paste0(
        ifelse(sign(x[i, "tzhour"]), "+", "-"),
        format_field(x[i, "tzhour"] %/% 1, 2, fmt = "%02.f"),
        pillar::style_subtle(":"),
        format_field(x[i, "tzhour"] %%  1 * 60, 2, fmt = "%02.f")
      )

    } else {
      x_str[, "tzhour"] <- ""
    }
  }

  x_str[x_omit] <- ""
  apply(x_str, 1, paste0, collapse = "")
}



#' @importFrom pillar style_subtle style_na
format_field <- function(x, digits = 2, leading_optional = FALSE,
    fmt = if (leading_optional) "%.f" else sprintf("%%0%.f.f", digits)) {

  paste0(
    if (leading_optional)
      pillar::style_subtle(strrep("0", digits - nchar(x %|NA|% 0L))),
    ifelse(is.na(x), pillar::style_na(sprintf(fmt, 0L)), sprintf(fmt, x)))
}

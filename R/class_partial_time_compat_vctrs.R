#' Full parttime class name
#'
#' @param x A `partial_time` object
#' @inheritParams vctrs::vec_ptype_full
#'
#' @return A `character` representation of the `partial_time` class name
#'
#' @importFrom vctrs vec_ptype_full
#' @exportS3Method vctrs::vec_ptype_full partial_time
vec_ptype_full.partial_time <- function(x, ...) {
  "partial_time"
}



#' Abbreviated partial time class name
#'
#' @param x A `partial_time` object
#' @inheritParams vctrs::vec_ptype_abbr
#'
#' @return A `character` representation of the abbreviated `partial_time` class
#'   name
#'
#' @importFrom vctrs vec_ptype_abbr
#' @exportS3Method vctrs::vec_ptype_abbr partial_time
vec_ptype_abbr.partial_time <- function(x, ..., prefix_named, suffix_shape) {
  "pttm"
}



#' parttime output header
#'
#' @param x A `partial_time` object
#' @param ... Additional arguments unused
#'
#' @return A `character` representation of `partial_time` metadata, as used to
#'   describe its vector output header
#'
#' @importFrom vctrs obj_print_header
#' @exportS3Method vctrs::obj_print_header partial_time
obj_print_header.partial_time <- function(x, ...) {
  perc_complete <- apply(!is.na(vctrs::field(x, "pttm_mat")), 2, mean)

  # reduce a couple components down to single terms
  perc_complete["sec"] <- perc_complete["sec"]
  perc_complete["tzhour"] <- perc_complete["tzhour"]
  perc_complete <- perc_complete[datetime_parts]

  # get singular timezone if consistent across entire vector
  tzs <- tz_consensus(vctrs::field(x, "pttm_mat"))

  names(perc_complete) <- sapply(names(perc_complete), switch,
    "year"    = "Y",
    "month"   = "M",
    "day"     = "D",
    "hour"    = "h",
    "min"     = "m",
    "sec"     = "s",
    "tzhour"  =
      if (identical(tzs, FALSE)) "+tz"  # no consensus timezone
      else if (tzs == 0) "Z"
      else if (tzs %% 100 == 0) sprintf("%+d", tzs / 100)
      else sprintf("%+05d", tzs),
    "")

  # filter out any unusable fields - when length is 0
  perc_complete[!is.finite(perc_complete)] <- NA

  # style header summary of missingness
  names(perc_complete) <- ifelse(
    is.na(perc_complete), "", ifelse(
    perc_complete == 0, pillar::style_na(names(perc_complete)), ifelse(
    perc_complete == 1, names(perc_complete),
    pillar::style_subtle(names(perc_complete)))))

  # format header
  complete_txt <- paste0(names(perc_complete), collapse = "")
  cat(sprintf("<%s%s[%.f]>",
    vctrs::vec_ptype_full(x),
    if (nchar(complete_txt)) paste0("<", complete_txt, ">") else "",
    length(x)), "\n")

  invisible(x)
}



#' parttime data output
#'
#' @param x A `partial_time` object
#' @param ... Additional arguments unused
#'
#' @return A `character` representation of `partial_time`
#'
#' @importFrom vctrs obj_print_data
#' @exportS3Method vctrs::obj_print_data partial_time
obj_print_data.partial_time <- function(x, ...) {
  if (!length(x)) return(invisible(x))
  cat(format_vector(format(x, ...)), "\n")
  invisible(x)
}



#' parttime footer
#'
#' @param x A `partial_time` object
#' @param ... Additional arguments unused
#'
#' @return A string output when `partial_time` vector printing exceeds max print
#'   length.
#'
#' @importFrom vctrs obj_print_footer
#' @exportS3Method vctrs::obj_print_footer partial_time
obj_print_footer.partial_time <- function(x, ...) {
  l <- length(x) - getOption("max.print")
  if (l > 0) {
    cat(sprintf('[ reached getOption("max.print") -- omitted %.f entries ]', l))
  }

  invisible(x)
}



tz_consensus <- function(xmat) {
  if (nrow(xmat) < 1 || "tzhour" %in% colnames(xmat))
    return(FALSE)

  tzs <- xmat[, "tzhour"]
  if (isTRUE(all(tzs == tzs[1]))) tzs[[1]] else FALSE
}

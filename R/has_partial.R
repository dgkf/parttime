#' The datetime components a partial_time carries
#'
#' Taken from the object rather than restated as a second list, so the two
#' cannot drift apart.
#'
#' @param x a partial_time object
#' @return A character vector of component names.
#' @noRd
partial_time_components <- function(x) {
  colnames(vctrs::field(x, "pttm_mat"))
}



#' Test whether a partial_time object is incomplete
#'
#' @param x a partial_time object to test for incompleteness
#' @param ... additional arguments unused
#' @param components components to include in testing. Defaults to every
#'   component the object carries.
#'
#' @return A logical vector indicating whether each element of a `partial_time`
#'   has any missing datetime fields.
#'
#' @examples
#' has_partial(as.parttime(c("2015", "2015-04-13T10:30:15")))
#' has_partial(as.parttime("2015-04"), "year", "month")
#'
#' @export
has_partial <- function(x, ..., components = NULL) {

  dots <- as.list(match.call())[-1]
  dots <- as.character(dots[!names(dots) %in% names(formals())])

  if (length(dots)) components <- dots

  available <- partial_time_components(x)
  if (is.null(components)) components <- available

  unknown <- setdiff(components, available)
  if (length(unknown)) {
    stop(sprintf(
      "`components` must name components of a partial_time (%s), not %s.",
      paste(available, collapse = ", "),
      paste(unknown, collapse = ", ")
    ))
  }

  apply(
    vctrs::field(x, "pttm_mat")[, components, drop = FALSE],
    1,
    function(row) any(is.na(row))
  )
}



#' Test whether a partial_time object's date components are incomplete
#'
#' @inheritParams has_partial
#'
#' @return A logical vector indicating whether each element of a `partial_time`
#'   has any missing date fields.
#'
#' @examples
#' has_partial_date(as.parttime(c("2015-04", "2015-04-13")))
#'
#' @export
has_partial_date <- function(x) {
  has_partial(x, components = c("year", "month", "day", "tzhour"))
}



#' Test whether a partial_time object's time components are incomplete
#'
#' @inheritParams has_partial
#'
#' @return A logical vector indicating whether each element of a `partial_time`
#'   has any missing time fields.
#'
#' @examples
#' has_partial_time(as.parttime(c("2015-04-13", "2015-04-13T10:30:15")))
#'
#' @export
has_partial_time <- function(x) {
  has_partial(x, components = c("hour", "min", "sec", "tzhour"))
}

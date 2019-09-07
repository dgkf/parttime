#' @export
is_partial <- function(x, ..., components = c("year", "month", "day", "hour",
    "min", "sec", "secfrac", "tzhour", "tzmin")) {

  dots <- as.list(match.call())[-1]
  dots <- as.character(dots[!names(dots) %in% names(formals())])

  if (length(dots)) components <- dots

  match.arg(components, c("year", "month", "day", "hour", "min", "sec", 
      "secfrac", "tzhour", "tzmin"), several.ok = TRUE)

  apply(
    vctrs::field(x, "pttm_mat")[,components, drop = FALSE], 
    1, 
    function(row) any(is.na(row)))
}

#' @export
is_partial_date <- function(x, ...) {
  is_partial(x, ..., components = c("year", "month", "day", "tzhour", "tzmin"))
}

#' @export
is_partial_time <- function(x, ...) {
  is_partial(x, ..., components = c("hour", "min", "sec", "secfrac", "tzhour", 
      "tzmin"))
}

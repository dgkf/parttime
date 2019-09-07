get_field <- function(x, field) {
  vctrs::field(x, "pttm_mat")[,field]
}

curry_field_f <- function(field) {
  function(x) get_field(x, field)
}

#' @export
year.partial_time <- curry_field_f("year")

#' @export
month.partial_time <- curry_field_f("month")

#' @export
day.partial_time <- curry_field_f("day")

#' @export
hour.partial_time <- curry_field_f("hour")

#' @export
minute.partial_time <- curry_field_f("min")

#' @export
min.partial_time <- curry_field_f("min")

#' @export
second.partial_time <- curry_field_f("sec")

#' @export
sec.partial_time <- curry_field_f("sec")

#' @export
secfrac <- function(x) UseMethod("secfrac")

#' @export
secfrac.partial_time <- curry_field_f("secfrac")

#' @export
tzhour <- function(x) UseMethod("tzhour")

#' @export
tzhour.partial_time <- curry_field_f("tzhour")

#' @export
tzmin <- function(x) UseMethod("tzmin")

#' @export
tzmin.partial_time <- curry_field_f("tzmin")

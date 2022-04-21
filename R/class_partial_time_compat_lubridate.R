#' Wrapper for lubridate as.interval
#'
#' @inheritParams lubridate::as.interval
#'
#' @importFrom methods setMethod signature
#' @importFrom lubridate as.interval
#' @export
methods::setMethod("as.interval", methods::signature("partial_time"), function(x, ...) {
  vctrs::vec_cast(x, structure(0L, class = "timespan"))
})

get_field <- function(x, field) {
  vctrs::field(x, "pttm_mat")[,field]
}

curry_field_f <- function(field) {
  function(x) get_field(x, field)
}

#' @importFrom lubridate year
#' @export
year.partial_time <- curry_field_f("year")

#' @importFrom lubridate month
#' @export
month.partial_time <- curry_field_f("month")

#' @importFrom lubridate day
#' @export
day.partial_time <- curry_field_f("day")

#' @importFrom lubridate hour
#' @export
hour.partial_time <- curry_field_f("hour")

#' @importFrom lubridate minute
#' @export
minute.partial_time <- curry_field_f("min")

#' @importFrom lubridate second
#' @export
second.partial_time <- function(x) {
  get_field(x, "sec") + get_field(x, "secfrac")
}

#' @importFrom lubridate tz
#' @export
tz.partial_time <- function(x) {
  get_field(x, "tzhour") * 60 + get_field(x, "tzmin")
}

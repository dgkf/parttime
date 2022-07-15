#' @importFrom lubridate as.interval
methods::setGeneric("as.interval", lubridate::as.interval)

#' Wrapper for lubridate as.interval
#'
#' @inheritParams lubridate::as.interval
#'
#' @importFrom methods setMethod signature
#' @importFrom lubridate as.interval
#' @export
methods::setMethod(
  "as.interval",
  methods::signature("partial_time"),
  function(x, ...) {
    vctrs::vec_cast(x, structure(0L, class = "timespan"))
  }
)



get_field <- function(x, field) {
  vctrs::field(x, "pttm_mat")[, field]
}

set_field <- function(x, fields, value) {
  vctrs::field(x, "pttm_mat")[, fields] <- value
  x
}

gen_get_field_fn <- function(field) {
  f <- function(x) {}
  body(f) <- bquote(get_field(x, .(field)))
  environment(f) <- parent.env(environment())
  f
}

gen_set_field_fn <- function(field) {
  f <- function(x, value) {}
  body(f) <- bquote(set_field(x, .(field), value))
  environment(f) <- parent.env(environment())
  f
}

gen_set_field_warn_s4 <- function(fname) {
  f <- function(x, value) {}
  body(f) <- bquote({
    stop(sprintf("`%s` not defined for type `%s`", .(fname), class(x)[[1L]]))
  })
  environment(f) <- parent.env(environment())
  f
}



#' Datetime component access and assignment functions
#'
#' @note
#' Care is taken to make these functionas as compatible as possible with similar
#' datetime packages. However, some functions may be masked and cause errors
#' using their masking functions.
#'
#' @param x A time-like object to access or assign to
#' @param value For assignment, a value to assign
#'
#' @name parttime_access_and_assign
#' @rdname parttime_access_and_assign
#'
#' @export
year <- function(x) UseMethod("year")

#' @rdname parttime_access_and_assign
#' @export
`year<-` <- function(x, value) UseMethod("year<-")

#' @rdname parttime_access_and_assign
#' @export
month <- function(x) UseMethod("month")

#' @rdname parttime_access_and_assign
#' @export
`month<-` <- function(x, value) UseMethod("month<-")

#' @rdname parttime_access_and_assign
#' @export
mday <- function(x) UseMethod("mday")

#' @rdname parttime_access_and_assign
#' @export
`mday<-` <- function(x, value) UseMethod("mday<-")

#' @rdname parttime_access_and_assign
#' @export
day <- mday

#' @rdname parttime_access_and_assign
#' @export
`day<-` <- `mday<-`

#' @rdname parttime_access_and_assign
#' @export
hour <- function(x) UseMethod("hour")

#' @rdname parttime_access_and_assign
#' @export
`hour<-` <- function(x, value) UseMethod("hour<-")

#' @rdname parttime_access_and_assign
#' @export
minute <- function(x) UseMethod("minute")

#' @rdname parttime_access_and_assign
#' @export
`minute<-` <- function(x, value) UseMethod("minute<-")

#' @rdname parttime_access_and_assign
#' @export
second <- function(x) UseMethod("second")

#' @rdname parttime_access_and_assign
#' @export
`second<-` <- function(x, value) UseMethod("second<-")

#' @rdname parttime_access_and_assign
#' @export
tz <- function(x) UseMethod("tz")

#' @rdname parttime_access_and_assign
#' @export
`tz<-` <- function(x, value) UseMethod("tz<-")



#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::year,partial_time)
#' @export
year.partial_time <- gen_get_field_fn("year")

#' @rdname parttime_access_and_assign
#' @usage \\method{year}{partial_time}(x) <- value
#' @export
`year<-.partial_time` <- gen_set_field_fn("year")

#' @rdname parttime_access_and_assign
#' @importMethodsFrom lubridate year<-
#' @export
methods::setMethod(
  "year<-",
  methods::signature("partial_time"),
  gen_set_field_fn("year")
)



#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::month,partial_time)
#' @export
month.partial_time <- gen_get_field_fn("month")

#' @rdname parttime_access_and_assign
#' @usage \\method{month}{partial_time}(x) <- value
#' @export
`month<-.partial_time` <- gen_set_field_fn("month")

#' @rdname parttime_access_and_assign
#' @importMethodsFrom lubridate month<-
#' @export
methods::setMethod(
  "month<-",
  methods::signature("partial_time"),
  gen_set_field_fn("month")
)



#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::mday,partial_time)
#' @export
mday.partial_time <- gen_get_field_fn("day")

#' @rdname parttime_access_and_assign
#' @usage \\method{day}{partial_time}(x) <- value
#' @export
`day<-.partial_time` <- gen_set_field_fn("day")

#' @rdname parttime_access_and_assign
#' @importMethodsFrom lubridate day<-
#' @export
methods::setMethod(
  "day<-",
  methods::signature("partial_time"),
  gen_set_field_fn("day")
)



#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::hour,partial_time)
#' @export
hour.partial_time <- gen_get_field_fn("hour")

#' @rdname parttime_access_and_assign
#' @usage \\method{hour}{partial_time}(x) <- value
#' @export
`hour<-.partial_time` <- gen_set_field_fn("hour")

#' @rdname parttime_access_and_assign
#' @importMethodsFrom lubridate hour<-
#' @export
methods::setMethod(
  "hour<-",
  methods::signature("partial_time"),
  gen_set_field_fn("hour")
)



#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::minute,partial_time)
#' @export
minute.partial_time <- gen_get_field_fn("min")

#' @rdname parttime_access_and_assign
#' @usage \\method{minute}{partial_time}(x) <- value
#' @export
`minute<-.partial_time` <- gen_set_field_fn("min")

#' @rdname parttime_access_and_assign
#' @importMethodsFrom lubridate minute<-
#' @export
methods::setMethod(
  "minute<-",
  methods::signature("partial_time"),
  gen_set_field_fn("min")
)



#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::second,partial_time)
#' @export
second.partial_time <- function(x) {
  get_field(x, "sec") + get_field(x, "secfrac")
}

#' @rdname parttime_access_and_assign
#' @usage \\method{second}{partial_time}(x) <- value
#' @export
`second<-.partial_time` <- function(x, value) {
  sec <- trunc(value)
  secfrac <- value - sec
  set_field(x, c("sec", "secfrac"), cbind(sec = sec, secfrac = secfrac))
}

#' @rdname parttime_access_and_assign
#' @importMethodsFrom lubridate second<-
#' @export
methods::setMethod(
  "second<-",
  methods::signature("partial_time"),
  gen_set_field_fn("sec")
)



## tz<- will currently get masked by non S3 lubridate alternative

#' @rdname parttime_access_and_assign
#' @rawNamespace S3method(lubridate::tz,partial_time)
#' @export
tz.partial_time <- function(x) {
  get_field(x, "tzhour") * 60 + get_field(x, "tzmin")
}

#' @rdname parttime_access_and_assign
#' @usage \\method{tz}{partial_time}(x) <- value
#' @export
`tz<-.partial_time` <- function(x, value) {
  set_field(x, c("tzhour", "tzmin"), cbind(tzhour = value %/% 60, tzmin = value %% 60))
}



## Extending lubridate style `+` with Period

#' Addition of a lubridate Period to a parttime partial_time
#'
#' @param e1 A partial_time object
#' @param e2 A lubridate Period object
#'
#' @export
methods::setMethod(
  "+",
  methods::signature("partial_time", "Period"),
  function(e1, e2) {
    # prevent lubridate NA <Period> from clobbering all fields
    if (is.na(e2)) return(e1)

    e2_secs <- trunc(e2)

    vctrs::field(e1, "pttm_mat")[, "year"]    <- year(e1)   + attr(e2, "year")
    vctrs::field(e1, "pttm_mat")[, "month"]   <- month(e1)  + attr(e2, "month")
    vctrs::field(e1, "pttm_mat")[, "day"]     <- day(e1)    + attr(e2, "day")
    vctrs::field(e1, "pttm_mat")[, "hour"]    <- hour(e1)   + attr(e2, "hour")
    vctrs::field(e1, "pttm_mat")[, "min"]     <- minute(e1) + attr(e2, "minute")
    vctrs::field(e1, "pttm_mat")[, "sec"]     <- second(e1) + e2_secs
    vctrs::field(e1, "pttm_mat")[, "secfrac"] <- get_field(e1, "secfrac") + (e2 - e2_secs)
    vctrs::field(e1, "pttm_mat") <- reflow_fields(vctrs::field(e1, "pttm_mat"))

    e1
  }
)

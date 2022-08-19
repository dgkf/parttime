#' @importFrom methods setClass
methods::setClass("partial_time")



#' Create a parttime object
#'
#' @param year numeric vector to use for partial time year component
#' @param month numeric vector to use for partial time month component
#' @param day numeric vector to use for partial time day component
#' @param hour numeric vector to use for partial time hour component
#' @param min numeric vector to use for partial time min component
#' @param sec numeric vector to use for partial time sec component
#' @param tzhour numeric vector to use for partial time tzhour component
#'
#' @examples
#' parttime(2019)
#'
#' @export
parttime <- function(
  year = NA, month = NA, day = NA, hour = NA, min = NA, sec = NA,
  tzhour = interpret_tz(getOption("parttime.assume_tz_offset", NA)) / 60
) {
  # handle special case when no arguments are provided
  args <- as.list(sys.call()[-1])
  if (!length(args)) return(parttime(NA)[0])

  fields <- vctrs::vec_recycle_common(year, month, day, hour, min, sec, tzhour)
  names(fields) <- names(formals())

  l <- length(fields[[1]])
  as.parttime(matrix(
    unlist(lapply(fields, as.numeric)),
    ncol = length(fields),
    dimnames = list(rep(NA_character_, l), names(fields))
  ))
}

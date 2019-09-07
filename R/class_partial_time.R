#' @export
#'
#' @examples
#' parttime(2019)
#' 
parttime <- function(year = NA, month = NA, day = NA, hour = NA, min = NA, 
    sec = NA, 
    tzhour = interpret_tz(getOption('parttime.assume_tz_offset', NA)) %/% 60,
    tzmin  = interpret_tz(getOption('parttime.assume_tz_offset', NA)) %% 60) {
  
  # handle special case when no arguments are provided
  args <- as.list(sys.call()[-1])
  if (!length(args)) return(parttime(NA)[0])
  
  secfrac <- sec %% 1
  sec <- sec %/% 1
  
  fields <- setNames(vctrs::vec_recycle_common(
      year, month, day, hour, min, sec, secfrac, tzhour, tzmin
    ), append(names(formals()), "secfrac", 6))
  
  l <- length(fields[[1]])
  as.parttime(matrix(
    unlist(lapply(fields, as.numeric)), 
    ncol = length(fields), 
    dimnames = list(rep(NA_character_, l), names(fields))
  ))
}

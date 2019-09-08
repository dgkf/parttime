#' @importFrom vctrs vec_ptype_full 
vec_ptype_full.timespan <- function(x) "timespan"



#' @importFrom vctrs vec_ptype_abbr 
vec_ptype_abbr.timespan <- function(x) "tmspn"



#' @importFrom vctrs obj_print_data
obj_print_data.timespan <- function(x, ...) {
  if (!length(x)) return(invisible(x))
  cat(format_vector(format(x, ...)), '\n')
  invisible(x)
}



#' @importFrom vctrs obj_print_footer
obj_print_footer.timespan <- function(x, ...) {
  l <- length(x) - getOption("max.print")
  if (l > 0) 
    cat(sprintf('[ reached getOption("max.print") -- omitted %.f entries ]', l))
}

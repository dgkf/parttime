#' @export
vec_ptype_full.timespan <- function(x) "timespan"



#' @export
vec_ptype_abbr.timespan <- function(x) "tmspn"



#' @export
obj_print_data.timespan <- function(x, ...) {
  if (!length(x)) return(invisible(x))
  cat(format_vector(format(x, ..., use_crayon = use_crayon())), '\n')
  invisible(x)
}



#' @export
obj_print_footer.timespan <- function(x, ...) {
  l <- length(x) - getOption("max.print")
  if (l > 0) 
    cat(sprintf('[ reached getOption("max.print") -- omitted %.f entries ]', l))
}

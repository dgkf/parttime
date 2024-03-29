#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.timespan <- function(x, ...) {
  "timespan"
}



#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.timespan <- function(x, ..., prefix_named, suffix_shape) {
  "tmspn"
}



#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.timespan <- function(x, ...) {
  if (!length(x)) return(invisible(x))
  cat(format_vector(format(x, ...)), "\n")
  invisible(x)
}



#' @importFrom vctrs obj_print_footer
#' @export
obj_print_footer.timespan <- function(x, ...) {
  l <- length(x) - getOption("max.print")
  if (l > 0)
    cat(sprintf("[ reached getOption(\"max.print\") -- omitted %.f entries ]", l))
}

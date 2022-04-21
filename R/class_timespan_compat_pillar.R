#' @importFrom pillar type_sum
#' @export
type_sum.timespan <- function(x) {
  "tmspn"
}



#' @importFrom pillar pillar_shaft
#' @importFrom crayon col_nchar
#' @export
pillar_shaft.timespan <- function(x, ...) {
  x_fmt <- format(x, ..., quote = FALSE)

  pillar::new_pillar_shaft(
    x_fmt,
    width = max(crayon::col_nchar(x_fmt)),
    subclass = "pillar_shaft_timespan")
}



#' @export
format.pillar_shaft_timespan <- function(x, width, ...) {
  pillar::new_ornament(x, width = width, align = "left")
}

#' parttime type name
#'
#' @inheritParams pillar::type_sum
#'
#' @return A `character` scalar shorthand representation of the `partial_time`
#'   class name
#'
#' @importFrom pillar type_sum
#' @export
type_sum.partial_time <- function(x) {
  "pttm"
}



#' parttime as pillar shaft
#'
#' @inheritParams pillar::new_pillar_shaft
#'
#' @return A `character` representation of a `partial_time` vector
#'
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.partial_time <- function(x, ...) {
  x_fmt <- format(x, ..., quote = FALSE)

  pillar::new_pillar_shaft(
    x_fmt,
    width = max(crayon::col_nchar(x_fmt)),
    subclass = "pillar_shaft_partial_time")
}



#' parttime pillar formattin
#'
#' @param width A maximum display width of the each element in the resulting
#'   vector of strings
#' @param ... Additional arguments unused
#' @inheritParams pillar::new_ornament
#'
#' @return A `character` representation of a `partial_time` vector
#'
#' @export
format.pillar_shaft_partial_time <- function(x, width, ...) {
  pillar::new_ornament(x, width = width, align = "left")
}

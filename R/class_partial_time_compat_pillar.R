#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.partial_time <- function(x) {
  TRUE
}



#' @importFrom pillar type_sum
#' @export
type_sum.partial_time <- function(x) {
  "pttm"
}



#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.partial_time <- function(x, ...) {
  x_fmt <- format(x, ..., quote = FALSE)
  
  pillar::new_pillar_shaft(
    x_fmt, 
    width = max(crayon::col_nchar(x_fmt)), 
    subclass = "pillar_shaft_partial_time")
}



#' @export
format.pillar_shaft_partial_time <- function(x, width, ...) {
  pillar::new_ornament(x, width = width, align = "left")
}
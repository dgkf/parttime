print.partial_difftime <- function(x, ...) {
  print(attr(x, "fields"))
  print(attr(x, "uncert"))
}

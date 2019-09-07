cols <- function(x, dim, ...) {
  which(dimnames(x)[[dim]] %in% c(...))
}
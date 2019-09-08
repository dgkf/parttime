partial_difftime <- function(fmat, umat) {
  # Data structure overview:
  #  - logical stub vector for C representation (see parttime)
  #  - "fields" matrix (n x 9) of integer or NA, similar to parttime "fields"
  #  - "uncert" matrix (n x 2) of value uncertainty in last field before NA and
  #    first NA field

  
  
  structure(
    vector("logical", nrow(fmat)),
    fields = fmat,
    uncert = umat,
    class = "partial_difftime")
}

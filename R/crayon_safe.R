has_crayon <- requireNamespace("crayon", quietly = TRUE)

use_crayon <- function() {
  has_crayon && !isFALSE(getOption("parttime.use_crayons"))
}

style_subtle_safe <- function(...) {
  if (use_crayon()) pillar::style_subtle(...)
  else identity(...)
}

style_na_safe <- function(...) {
  if (use_crayon()) pillar::style_na(...)
  else strrep("_", nchar(identity(...)))
}

substring_safe <- function(...) {
  if (use_crayon()) crayon::col_substring(...)
  else substring(...)
}
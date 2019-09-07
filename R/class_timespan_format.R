#' @export
format.timespan <- function(x, ..., use_crayon = FALSE, quote = TRUE, 
    verbose = getOption("parttime.print_verbose")) {
  
  x_str <- vector("character", length(x))
  x_str[] <- style_na_safe('NA')
  
  xna <- is.na(x)
  x_nna <- x[!xna]
  x_mat <- vctrs::field(x_nna, "tmspn_arr")
  inc <- .i(x_mat, 2, "inclusive")
 
  x_str[!xna] <- sprintf("%s%s %s %s%s", 
    style_subtle_safe(ifelse(x_mat[,"inclusive",1], "[", "(")),
    format_field_matrix(extract(x_mat,,-inc,"lb", drop = 3)), 
    style_subtle_safe(if (use_crayon) "\u2014" else '-'),
    format_field_matrix(extract(x_mat,,-inc,"ub", drop = 3)),
    style_subtle_safe(ifelse(x_mat[,"inclusive",2], "]", ")")))

  x_str
}
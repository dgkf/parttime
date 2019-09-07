#' @export
#' @importFrom pillar style_subtle style_na
format.timespan <- function(x, ..., quote = TRUE, 
    verbose = getOption("parttime.print_verbose", FALSE)) {
  
  x_str <- vector("character", length(x))
  x_str[] <- pillar::style_na('NA')
  
  xna <- is.na(x)
  x_nna <- x[!xna]
  x_mat <- vctrs::field(x_nna, "tmspn_arr")
  inc <- .i(x_mat, 2, "inclusive")
 
  x_str[!xna] <- sprintf("%s%s %s %s%s", 
    pillar::style_subtle(ifelse(x_mat[,"inclusive",1], "[", "(")),
    format_field_matrix(extract(x_mat,,-inc,"lb", drop = 3)), 
    pillar::style_subtle("\u2014"),
    format_field_matrix(extract(x_mat,,-inc,"ub", drop = 3)),
    pillar::style_subtle(ifelse(x_mat[,"inclusive",2], "]", ")")))

  x_str
}
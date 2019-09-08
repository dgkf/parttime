#' Subsetting assignment of partial_time objects
#' 
#' @inheritParams base::`[<-`
#' @param ... arguments unused 
#'
#' @examples
#' x <- as.parttime(c("2019", "2019-02", "2019-02-02"))
#' x[c(1, 3)] <- as.parttime(c("2000", "1999"))
#'
#' @export
`[<-.partial_time` <- function(x, i, ..., value) {
  # handle empty case
  if (is.numeric(i) && !length(i)) return(x)
  else if (is.logical(i) && !any(i)) return(x)
  
  value <- vctrs::vec_recycle(as.parttime(value), size = length(x[i]))
  vctrs::field(x, "pttm_mat")[i,] <- vctrs::field(value, "pttm_mat")
  
  # clean up rownames
  if (is.null(rownames(vctrs::field(x, "pttm_mat")))) 
    rownames(vctrs::field(x, "pttm_mat")) <- rep(NA_character_, length(x))
  rownames(vctrs::field(x, "pttm_mat"))[i] <- rownames(vctrs::field(value, "pttm_mat"))
  
  x
}



#' @export
`[[<-.partial_time` <- function(x, i, ..., value) {
  x[i] <- value
  x
}

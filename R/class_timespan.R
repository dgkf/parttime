#' @export
timespan <- function(start, end, inclusive = c(TRUE, FALSE)) {
  common_size <- vctrs::vec_size_common(start, end)
  
  inclusive <- matrix(
    rep_len(inclusive, common_size * 2),
    ncol = 2,
    byrow = TRUE)
  
  start <- as.parttime(start)
  start <- vctrs::vec_recycle(start, common_size)
  
  end <- as.parttime(end)
  end <- vctrs::vec_recycle(end, common_size)
  
  xdim <- dim(vctrs::field(start, "pttm_mat"))
  xdimnames <- dimnames(vctrs::field(start, "pttm_mat"))
  
  as.timespan(array(
    c(vctrs::field(start, "pttm_mat"), 
      inclusive[,1],
      vctrs::field(end, "pttm_mat"), 
      inclusive[,2]), 
    dim = c(xdim[[1]], xdim[[2]] + 1, 2),
    dimnames = list(
      xdimnames[[1]], 
      c(xdimnames[[2]], "inclusive"), 
      c("lb", "ub"))))
}
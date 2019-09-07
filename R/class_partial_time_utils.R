minimally_increment <- function(x) {
  x_mat <- vctrs::field(x, "pttm_mat")
  
  # increment last available time field
  last_indx <- which(col(x_mat) == apply(x_mat, 1, Position, f = is.na) - 1)
  x_mat[last_indx] <- x_mat[last_indx] + 1
  
  vctrs::field(x, "pttm_mat") <- reflow_fields(x_mat)
  x
}


time_min <- memoise::memoize(function() {
  as.parttime("0000-01-01T00:00:00.000-1200")
})

time_mid <- memoise::memoize(function() {
  as.parttime("0000-06-15T12:30:30.000+0000")
})

time_max <- memoise::memoize(function() {
  as.parttime("9999-12-31T23:59:59.999+1400")
})

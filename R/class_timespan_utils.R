cols <- function(x, dim, ...) {
  which(dimnames(x)[[dim]] %in% c(...))
}



complete_timespan <- function(x) {
  vctrs::field(x, "tmspn_arr")[,,"lb"] <- impute_time_min(extract(vctrs::field(x, "tmspn_arr"),,,"lb",drop = 3))
  
  inc <- vctrs::field(x, "tmspn_arr")[,"inclusive","ub"] == 1
  vctrs::field(x, "tmspn_arr")[inc,,"ub"] <- impute_time_min(minimally_increment(extract(vctrs::field(x, "tmspn_arr"),inc,,"ub",drop = 3)), tz = "+1400")
  vctrs::field(x, "tmspn_arr")[!inc,,"ub"] <- impute_time_min(extract(vctrs::field(x, "tmspn_arr"),!inc,,"ub",drop = 3), tz = "+1400")
  vctrs::field(x, "tmspn_arr")[inc,"inclusive","ub"] <- 0
  
  x
}
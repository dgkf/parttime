minimally_increment <- function(x) {
  UseMethod("minimally_increment")
}


minimally_increment.partial_time <- function(x) {
  x_mat <- vctrs::field(x, "pttm_mat")

  # increment last available time field
  col_tz <- colnames(x_mat) %in% c("tzhour", "tzmin")
  last_indx <- which(col(x_mat) == apply(x_mat, 1, Position, f = is.na) - 1)
  x_mat[, !col_tz][last_indx] <- x_mat[, !col_tz][last_indx] + 1

  vctrs::field(x, "pttm_mat") <- reflow_fields(x_mat)
  x
}


minimally_increment.matrix <- function(x_mat) {
  # increment last available time field
  col_inc <- colnames(x_mat) %in% "inclusive"
  col_tz <- colnames(x_mat) %in% c("tzhour", "tzmin")
  last_indx <- which(col(x_mat) == apply(x_mat, 1, Position, f = is.na) - 1)
  x_mat[, !col_tz & !col_inc][last_indx] <- x_mat[, !col_tz & !col_inc][last_indx] + 1
  x_mat[, !col_inc] <- reflow_fields(x_mat[, !col_inc, drop = FALSE])
  x_mat
}


time_min <- function() {
  as.parttime("0000-01-01T00:00:00.000")
}

time_mid <- function() {
  as.parttime("0000-06-15T12:30:30.000")
}

time_max <- function() {
  as.parttime("9999-12-31T23:59:59.999")
}

`%|NA|%` <- function(lhs, rhs) ifelse(is.na(lhs), rhs, lhs)



datetime_parts <- c("year", "month", "day", "hour", "min", "sec", "secfrac", "tzhour", "tzmin")



warn_partial <- function(x, ..., envir = parent.frame()) {
  fields <- c(...)
  if (!length(fields)) fields <- colnames(x)

  if (any(is.na(attr(x, "fields")[,fields])))
    warning(
      call. = FALSE,
      "In ", eval(deparse(sys.call(which = -1L)), envir = envir), " :\n",
      paste0(strwrap(indent = 2, paste0(
        "Coercing partial_time, which will assume an imputation method. ",
        "To avoid warnings, explicitly apply imputations with `impute_time()` ",
        "prior to coersion, specifying datetime fields ",
        spoken_list(fields, quote = TRUE),
        "."
        )), collapse = "\n"))
}



spoken_list <- function(x, quote = FALSE, oxford = FALSE) {
  if (isFALSE(quote)) quote <- ''
  else if (isTRUE(quote)) quote <- '"'

  paste0(
    if (length(x) > 1) paste0(quote, x[-length(x)], quote, collapse = ', '),
    if (length(x) > 1 && oxford) ",",
    if (length(x) > 1) " and ",
    paste0(quote, x[length(x)], quote))
}



gmtoff <- function(tzone) {
  if (is.numeric(tzone)) return(tzone)
  
  if (!tzone %in% OlsonNames())
    stop("Invalid timezone. Use `OlsonNames()` to see a list of valid timezones.")
  
  date_stub <- lubridate::as_datetime("1970-01-01 00:00:00 GMT")
  
  as.numeric(difftime(
    date_stub, 
    lubridate::force_tz(date_stub, tzone = tzone), 
    units = "secs")) / 60
}



local_tz <- function() {
  attr(as.POSIXlt(lubridate::now()), "tzone")
}



#' @importFrom utils head tail
#' @importFrom crayon col_nchar col_align
format_vector <- function(x) {
  xtrunc <- utils::head(x, getOption("max.print"))

  indxs <- paste0('[', seq_along(xtrunc), ']')
  max_indx_chr <- nchar(utils::tail(indxs, 1))
  x <- paste0(' ', x)

  max_chr <- max(crayon::col_nchar(x), 0, na.rm = TRUE)
  x <- crayon::col_align(x, max_chr)

  n_per_row <- max((getOption("width", 80) - max_indx_chr) %/% max_chr, 1)
  last_in_row_indx <-  seq_len((length(xtrunc) - 1) %/% n_per_row) * n_per_row
  first_in_row_indx <- seq(1, length(xtrunc), n_per_row)

  x[last_in_row_indx] <- paste0(x[last_in_row_indx], "\n")
  x[first_in_row_indx] <- sprintf(sprintf("%%%ds%%s", max_indx_chr),
    indxs[first_in_row_indx],
    x[first_in_row_indx])

  paste(x, collapse = '')
}



#' Similar to matrix and array `[` behavior, but allows for providing a numeric
#' vector of dimensions to drop
#'
#' @param x array or matrix to extract from
#' @param ... pased to \code{[}
#' @param drop whether to drop dimensions, a logical or numeric vector of
#'   dimensions to drop
#' @param envir environment in which to evaluate \code{[} call
#' 
extract <- function(x, ..., drop = TRUE, envir = parent.frame()) {
  args <- as.list(match.call())[-1]
  args <- args[names(args) == ""]
  
  vals <- sapply(args, class) != "name"
  length_dims <- sapply(args, length) * (as.character(args) != "")
  
  if (is.logical(drop))
    if (length(drop) == 1)
      return(do.call("[", append(append(list(x), args), list(drop = drop)), envir = envir))
    else if (length(drop) == length(dim(x)))
      drop <- which(drop)
  
  if (!is.numeric(drop))
    stop('argument `drop` must be either logical of length 1 or numeric')
  
  x <- do.call("[", append(append(list(x), args), list(drop = FALSE)), envir = envir)
  drop <- 1:length(dim(x)) %in% drop & dim(x) == 1
  dimnames_keep <- dimnames(x)[!drop]
  dim(x) <- dim(x)[!drop]
  dimnames(x) <- dimnames_keep
  x
}



#' shorthand for converting dimnames to indices, for easier column subtraction
#'
#' @param x an object with dimnames<- defined
#' @param dim the dimension to index
#' @param ... names selected by which
#' 
.i <- function(x, dim, ...) {
  which(dimnames(x)[[dim]] %in% c(...))
}
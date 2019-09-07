#' @export
format.partial_time <- function(x, ..., use_crayon = FALSE, quote = TRUE) {
  x_str <- vector("character", length(x))
  x_str[] <- style_na_safe('NA')
  
  xna <- is.na(x)
  x_str[!xna]<- sprintf(
    if (quote) '"%s"' else '%s',
    format_field_matrix(vctrs::field(x[!xna], "pttm_mat")))
  x_str
}



format_field_matrix <- function(x, 
    verbose = getOption("parttime.print_verbose", FALSE)) {
  
  tz <- .i(x, 2, "tzhour", "tzmin")
  
  x_omit <- FALSE
  if (isFALSE(verbose)) {
    x_omit <- matrix(logical(), 
      nrow = nrow(x), 
      ncol = ncol(x), 
      dimnames = dimnames(x))
    
    x_omit[,tz] <- all(is.na(x[,tz])) | all(x[,tz] == 0)
    x_omit[,-tz] <- col(x[,-tz,drop = FALSE]) > apply(x[,-tz,drop = FALSE], 1, Position, f = Negate(is.na), right = TRUE)
  }
  
  x_str <- matrix(character(), 
    nrow = nrow(x), 
    ncol = ncol(x), 
    dimnames = dimnames(x))
  
  # date
  x_str[,"year"] <- format_field(x[,"year"], 4)
  x_str[,"month"] <- paste0(
    style_subtle_safe('-'), 
    format_field(x[,"month"], 2, TRUE))
  x_str[,"day"] <- paste0(
    style_subtle_safe('-'), 
    format_field(x[,"day"], 2, TRUE))
  
  # time
  x_str[,"hour"] <- paste0(' ', format_field(x[,"hour"], 2, TRUE))
  x_str[,"min"] <- paste0(style_subtle_safe(':'), format_field(x[,"min"], 2))
  x_str[,"sec"] <- paste0(style_subtle_safe(':'), format_field(x[,"sec"], 2))
  x_str[,"secfrac"] <- paste0(
    style_subtle_safe('.'),
    substring_safe(format_field(x[,"secfrac"], 3, fmt = "%.03f"), 3))
  
  # optional timezone (timespan/duration have no tz elements)
  if ("tzhour" %in% colnames(x_str))
    x_str[,"tzhour"] <- format_field(x[,"tzhour"], 2, fmt = "%+03.f")
  if ("tzmin" %in% colnames(x_str))
    x_str[,"tzmin"] <- substring_safe(format_field(x[,"tzmin"], 2, fmt = "%+03.f"), 2)
  
  x_str[x_omit] <- ''
  apply(x_str, 1, paste0, collapse = '')
}



format_field <- function(x, digits = 2, leading_optional = FALSE, 
    fmt = if (leading_optional) "%.f" else sprintf("%%0%.f.f", digits)) {

  paste0(
    if (leading_optional) 
      style_subtle_safe(strrep('0', digits - nchar(x %|NA|% 0L))),
    ifelse(is.na(x), style_na_safe(sprintf(fmt, 0L)), sprintf(fmt, x)))
}

#' @export
vec_ptype_full.partial_time <- function(x) "partial_time"



#' @export
vec_ptype_abbr.partial_time <- function(x) "pttm"



#' @export
obj_print_header.partial_time <- function(x, ...) {
  perc_complete <- apply(!is.na(vctrs::field(x, "pttm_mat")), 2, mean)
  
  # reduce a couple components down to single terms
  perc_complete["sec"] <- min(perc_complete[c("sec", "secfrac")])
  perc_complete["tzhour"] <- min(perc_complete[c("tzhour", "tzmin")])
  perc_complete <- perc_complete[!names(perc_complete) %in% c("secfrac", "tzmin")]
  perc_complete <- perc_complete[datetime_parts]
  
  names(perc_complete) <- sapply(names(perc_complete), switch,
    "year"    = "Y",
    "month"   = "M",
    "day"     = "D",
    "hour"    = "h",
    "min"     = "m",
    "sec"     = "s",
    "tzhour"  = "+tz",
    "")
  
  # filter out any unusable fields - when length is 0
  perc_complete[!is.finite(perc_complete)] <- NA
  
  # style header summary of missingness
  names(perc_complete) <- ifelse(
    is.na(perc_complete), "", ifelse(
    perc_complete == 0, pillar::style_na(names(perc_complete)), ifelse(
    perc_complete == 1, names(perc_complete), 
    pillar::style_subtle(names(perc_complete)))))
  
  # format header
  complete_txt <- paste0(names(perc_complete), collapse = "")
  cat(sprintf("<%s%s[%.f]>",
    vctrs::vec_ptype_full(x), 
    if (nchar(complete_txt)) paste0("<", complete_txt, ">") else "",
    length(x)), '\n')
  
  invisible(x)
}



#' @export
obj_print_data.partial_time <- function(x, ...) {
  if (!length(x)) return(invisible(x))
  cat(format_vector(format(x, ...)), '\n')
  
  invisible(x)
}



#' @export
obj_print_footer.partial_time <- function(x, ...) {
  l <- length(x) - getOption("max.print")
  if (l > 0) 
    cat(sprintf('[ reached getOption("max.print") -- omitted %.f entries ]', l))
  
  invisible(x)
}

.onAttach <- function(libname, pkgname) {
  if (!'parttime.assume_tz' %in% names(.Options)) {
    options('parttime.assume_tz' = 'GMT')
    
    message(paste(strwrap(sprintf(paste0(
        'Initializing `options("parttime.assume_tz")` with "%s", which will ', 
        'assume the "GMT" timezone when timezone parts are missing.'), 
      options('parttime.assume_tz'))),
      collapse = '\n'))
  }
  
  if (!'parttime.print_verbose' %in% names(.Options))
    options('parttime.print_verbose' = FALSE)
}
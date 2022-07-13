apply_default_tz_offset <- function(verbose = FALSE) {
  if (is.null(getOption("parttime.assume_tz_offset", NULL))) {
    options("parttime.assume_tz_offset" = 0)
    if (!verbose) return()

    msg <- paste(
      strwrap(sprintf(
        paste0(
          "Initializing `options(\"parttime.assume_tz_offset\")` with `%s`, ",
          "which will assume a timezone offset when timezone parts are ",
          "missing.\n"
        ),
        getOption("parttime.assume_tz_offset")
      )),
      collapse = "\n"
    )

    packageStartupMessage(msg)
  }
}



.onAttach <- function(libname, pkgname) {
  apply_default_tz_offset(TRUE)
}



.onLoad <- function(libname, pkgname) {
  apply_default_tz_offset()
}

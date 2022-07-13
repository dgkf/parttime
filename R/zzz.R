.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("parttime.assume_tz_offset", NULL))) {
    options("parttime.assume_tz_offset" = 0)
    packageStartupMessage(paste(
      strwrap(sprintf(
        paste0(
          "Initializing `options(\"parttime.assume_tz_offset\")` with `%s`, ",
          "which will assume a timezone offset when timezone parts are ",
          "missing.\n"
        ),
        getOption("parttime.assume_tz_offset")
      )),
      collapse = "\n"
    ))
  }
}

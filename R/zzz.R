apply_default_tz_offset <- function() {
  if (is.null(getOption("parttime.assume_tz_offset", NULL))) {
    options("parttime.assume_tz_offset" = 0L)

    msg <- paste0(
      paste0(collapse = "\n", strwrap(paste0(
        "Initializing default timezone offset, which is assumed when ",
        "timezone parts are missing."
      ))),
      sprintf(
        "\n\n    options(\"parttime.assume_tz_offset\" = %s)\n",
        deparse(getOption("parttime.assume_tz_offset"))
      )
    )

    packageStartupMessage(msg)
  }
}

.onLoad <- function(libname, pkgname) {
  apply_default_tz_offset()
}

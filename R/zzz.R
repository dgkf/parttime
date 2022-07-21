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

#' Export S3 generics only if as-of-yet unknown
#'
#' S3 generics are often (unfortunately) tethered to specific packages. In order
#' to provide a functional generic that can dispatch on parttime classes, while
#' also preventing our generics from masking generics in other packages on
#' attach, we want to only export our generics if there is no generic with that
#' name.
#'
#' @keywords internal
#'
#' @importFrom utils packageName
#'
register_unknown_s3_generics <- function(fns) {
  namespaceExport(
    getNamespace(utils::packageName()),
    setdiff(fns, .knownS3Generics)
  )
}

.onLoad <- function(libname, pkgname) {
  apply_default_tz_offset()
  register_unknown_s3_generics(c("start", "end"))
}

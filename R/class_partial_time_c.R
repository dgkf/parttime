#' @export
c.partial_time <- function(...) {
  xs <- list(...)
  if (length(unique(lapply(xs, class))) != 1)
    stop("can't concatenate partial_time object with objects of another class")

  xs_wo_class <- xs

  for (i in seq_along(xs_wo_class))
    class(xs_wo_class[[i]]) <- setdiff(class(xs_wo_class[[i]]), "partial_time")

  structure(
    rep(0, do.call(sum, lapply(xs, nrow))),
    fields = do.call(rbind, lapply(xs, attr, "fields")),
    class = class(xs[[1]]))
}

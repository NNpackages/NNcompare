#' @title print.summary.comparedf
#' The summary method for a \code{comparedf} object
#'
#' Print a more detailed output of the \code{\link{comparedf}} object.
#'
#' @param x An object of class \code{"summary.comparedf"}, as made by the \code{\link{comparedf}} S3 method.
#' @param ... Other arguments passed to \code{\link{print.summary.comparedf}}. In \code{print}, these are passed to \code{\link[knitr]{kable}}.
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "pandoc".
#'   To use the default in \code{kable}, pass \code{NULL}.
#' @return An object of class \code{"summary.comparedf"} is returned.

#' @name print.summary.comparedf

#' @rdname print.summary.comparedf
#' @export
#' @importFrom arsenal  is.Date
print.summary.comparedf <- function(x, ..., format = "pandoc")
{
  orig <- x
  sumdiffs <- sum(x$diffs.byvar.table$n)
  ctrl <- x$control
  ctrl$max.print.vars.ns <- ctrl$max.print.vars
  ctrl$max.print.vars.nc <- ctrl$max.print.vars
  if (is.null(ctrl$max.print.diffs.per.var) || is.na(ctrl$max.print.diffs.per.var)) ctrl$max.print.diffs.per.var <- sumdiffs
  if (nrow(x$diffs.table) > 0)
  {
    x$diffs.table <- do.call(rbind, by(x$diffs.table, factor(x$diffs.table$var.x, levels = unique(x$diffs.table$var.x)),
                                       utils::head, ctrl$max.print.diffs.per.var))
    
    # Need this for knitr to output list-cols of factors and dates correctly
    as_char <- function(x) if (is.factor(x) || is.Date(x)) x <- as.character(x) else x
    x$diffs.table$values.x <- lapply(x$diffs.table$values.x, as_char)
    x$diffs.table$values.y <- lapply(x$diffs.table$values.y, as_char)
  }
  
  for (v in c("frame.summary", "comparison.summary", "vars.ns", "vars.nc", "obs", "diffs.byvar", "diffs", "attrs"))
  {
    obj <- x[[paste0(v, ".table")]]
    nprint <- ctrl[[paste0("max.print.", v)]]
    
    # there is purposefully no max.print.frame.summary or max.print.comparison.summary
    if (is.null(nprint) || is.na(nprint)) nprint <- nrow(obj)
    
    caption <- switch(
      v,
      frame.summary = "Summary of data.frames",
      comparison.summary = "Summary of overall comparison",
      vars.ns = "Variables not shared",
      vars.nc = "Other variables not compared",
      obs = "Observations not shared",
      diffs.byvar = "Differences detected by variable",
      diffs = "Differences detected",
      attrs = "Non-identical attributes"
    )
    if (nrow(obj) > 0)
    {
      if (v == "diffs" && sumdiffs > min(nprint, nrow(obj)))
      {
        caption <- paste0(caption, " (", sumdiffs - min(nprint, nrow(obj)), " not shown)")
      } else if (nrow(obj) > nprint)
      {
        caption <- paste0(caption, " (", nrow(obj) - nprint, " not shown)")
      }
      if (format == "markdown")  cat("\n##", caption, "\n")
      print(knitr::kable(utils::head(obj, nprint), format = format, caption = caption, row.names = FALSE, ...))
    } else
    {
      nocaption <- paste0("No ", tolower(caption))
      print(knitr::kable(data.frame(x = nocaption), format = format, caption = caption, row.names = FALSE, col.names = "", ...))
    }
    cat("\n")
  }
  invisible(orig)
}

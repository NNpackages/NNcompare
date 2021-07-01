#' Export method
#' 
#' @param x The object to export
#'
#' @param ... Additional parameters
#' 
#' @export
export <- function(x,...) {
  UseMethod("export")
}


#' The export method for a \code{summary.comparedf} object
#'
#' Exports the output of the \code{\link{summary.comparedf}} object to a file.
#'
#' @param ... Other arguments passed to \code{\link[rmarkdown]{render}}. In
#'   \code{print}, these are passed to \code{\link[knitr]{kable}}.
#' @param type Type of output - html, word or pdf.
#' @param db An \code{\link[NNaccess]{nnaccess}} object
#' @param folder The folder in the instance - default: parprog, but it could
#'   also be nonctrprog.
#' @param subfolder Optional subfolder to folder.
#' @param filename Name of file without extension, when omitted it defaults to
#'   the name of the script that executes the command.
#' @param title Title in the header of the document.
#' @param author Author of the document - default: the user that executes the
#'   program.
#' @param deleteRmd If TRUE the Rmd file will be deleted, else not.  
#' @param x An object returned by the \code{summary.comparedf} function.
#'
#' @seealso \code{\link{summary.comparedf}}, \code{\link{comparedf.control}}
#' @name export.summary.comparedf
#' @export
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown render 
#' @importFrom NNaccess is.nnaccess 
#' @importFrom NNexport getScriptName
export.summary.comparedf <-
  function(x,
           ...,
           type = c("html", "word", "pdf", "github"),
           db = NULL,
           folder = "parprog",
           subfolder = "",
           filename = "",
           title = "",
           author = Sys.info()[["user"]],
           deleteRmd = TRUE
           )
  {
    type <- match.arg(type)
    if (!is.null(db) && !NNaccess::is.nnaccess(db)) {
      stop("db object needs to an NNaccess::nnaccess object.")
    } 
    if (!is.null(db)) {
      paths <- attr(db, "nninst")$paths
      if (!folder %in% names(paths)) {
        
        msg <- paste("The specified folder is not in the db object.", 
                     "The following folders are found:", sep = "\n\n") 
        
        stop(paste0(msg, "\n\n", paste(names(paths), collapse = ", ")))    
      }
      path <- file.path(paths[folder], subfolder)
    }
    else {
      path <- file.path(folder, subfolder)
    }
    
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    authorline <- (paste("author: ", "\"",  author, "\"", sep = ""))
    dateline <- paste("date: ", "\"", date(), "\"", sep = "")
    format <- paste(type, "_document", sep = "")
    fileext <- tools::file_ext(NNexport::getScriptName())
    if (filename == "") {
      filename <- gsub("\\.", "_", NNexport::getScriptName())
      if (file.exists(filename))
        stop(paste0("The file: ", filename, " already exists"))
    }
    if (title == "") {
      titleline <- paste("title: ", "\"", "Output from parallel program: ", filename,".", fileext, "\"", sep = "")
    }
    else titleline <- (paste("title: ", "\"", title, "\"", sep = ""))
    rmdfile <- file.path(path, paste0(filename,".Rmd"))
    
    file.create
    fileConn <- file(rmdfile)
    writeLines(
      c(
        "---",
        titleline,
        authorline,
        dateline,
        "output:",
        "  pdf_document:",
        "    toc: true",
        "  html_document:",
        "    toc: true",
        "  word_document:",
        "    toc: true",
        "  github_document:",
        "    toc: true",
        "---",
        "```{r, echo=FALSE, results='asis'}",
        "print(x, format = \"markdown\")",
        "```"
      ),
      fileConn,
      sep = "\n"
    )
    close(fileConn)
    if (Sys.info()[1] == "Windows")
      rmdfile <- sub("//whale.novo.dk/projstat/*", "P://", rmdfile)
    
    rendered_file <- rmarkdown::render(rmdfile, output_format = format, ...)
    if (deleteRmd == TRUE) {
      on.exit(unlink(rmdfile))
    }
    outmsg <- paste("saved to:", path)
    message(outmsg)
    short.export.summary.comparedf(x = x, path = path, filename = filename, 
                                   rendered_file = rendered_file)
    return(invisible(x))
  }


#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @importFrom labelled set_variable_labels 
short.export.summary.comparedf <-
  function(x, path, filename, rendered_file) {
    
    short_table <-
      tibble(
        prog = filename,
        ncol = diff(x$frame.summary.table$ncol) != 0,
        nrow = diff(x$frame.summary.table$nrow) != 0,
        var  = nrow(x$diffs.table) > 0,
        summary = basename(rendered_file)
      ) %>%
      labelled::set_variable_labels(prog = "The program name", 
                                    ncol = "Difference in number of colums",
                                    nrow = "Difference in number of rows",
                                    var  = "Differences in variable values",
                                    summary = "Path to the summary document")
    
    class(short_table) <- c("short.summary.comparedf", class(short_table))
    
    saveRDS(short_table, file.path(path, paste0(filename, "_short_comparedf.rds")))
    
    return(invisible(x))
  }


# To avoid NOTEs the R CMD check
utils::globalVariables(".data")
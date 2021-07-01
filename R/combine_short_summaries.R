
#' Combine short summary files from exported summaries
#'
#' @param path \code{character} defining the path to the files that needs to be summarised
#' @param filename  \code{character} defining the name of the summary html output
#' @param open Should the generated summary file be opened
#'
#' @return creates an html table with combined summary info
#' @export
#' @importFrom data.table rbindlist
#' @importFrom dplyr rename
#' @importFrom htmltools tags
#' @importFrom DT datatable saveWidget
#' @importFrom rstudioapi viewer
combineDFComp <- function(path = ".", filename = "001_comparedf_summary", open = TRUE) {
  
  comp_files <-
    list.files(path = path,
               pattern = "_short_comparedf.rds",
               full.names = TRUE)
  
  ldata <- lapply(comp_files, readRDS)
  
  combdata <- data.table::rbindlist(ldata)
  
  combdata$summary <-
    paste0("<a href='",
           combdata$summary,
           "'>",
           "Go to detailed summary",
           "</a>")
  
  combdata_rename <- combdata %>% 
    dplyr::rename(Programme = "prog",
                  "Number of columns" = "ncol",
                  "Number of rows"    = "nrow",
                  "Compared values"   = "var",
                  "Open" = "summary")
  
  container_dt <- tags$table(class = 'display',
                              tags$thead(tags$tr(
                                tags$th('Programme', rowspan = 2),
                                tags$th(class = 'dt-center', colspan = 3, 'Difference in'),
                                tags$th('Open', rowspan = 2),
                                tags$tr(lapply(colnames(combdata_rename)[2:4], tags$th))
                                            )))
    
  
  y <- DT::datatable(combdata_rename, escape = FALSE, container = container_dt, rownames = FALSE, class = "",
                     options = list(autoWidth = TRUE,
                                    columnDefs = list(list(className = "dt-center", targets = "_all"))))

                    
  if (!is.null(filename) && !is.na(filename)) {
    DT::saveWidget(y, file.path(normalizePath(path), paste(filename, '.html')))
    if (open & interactive()) 
      rstudioapi::viewer(file.path(normalizePath(path), paste(filename, '.html')))
  }
  
  return(invisible(y))
}

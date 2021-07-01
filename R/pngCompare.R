#' Compare two png files
#'
#' @param file1 The path to file1  
#' @param file2 The path to file2 
#' @param n.white.parts The number of white parts to search for in footnote
#' @param n.white.lines The number of lines that constitutes a part
#' 
#' @return An object of class
#' @export
#' 
#' @examples 
#' # Make sure the training data is setup
#' NNtraining::createTrainingDB()
#' 
#' # create the paths to two png files that needs to be compared 
#' output_path = "~/training/nn1234/nn1234-4321/current/stats/output/"
#' flpageomean.png <- paste0(output_path, "flpageomean.png")
#' flpageomean_new.png <- paste0(output_path, "flpageomean_new.png")
#' 
#' # Compare the files
#' pngcompare <- pngCompare(file1 = flpageomean.png, file2 = flpageomean_new.png)
#' 
#' # Did we observe any differences
#' pngcompare
#' 
#' # where was the differences observed
#' plot(pngcompare)
#' 
#' @importFrom png readPNG
pngCompare <- function(file1, file2,  
                       n.white.parts = 3, n.white.lines = 4) {
  
  # read the png files
  png1 <- png::readPNG(file1, info = TRUE)
  png2 <- png::readPNG(file2, info = TRUE)
  
  # find the system footnote
  foot1 <- find_footnote(png1, n.white.parts = 2, n.white.lines = 4)
  foot2 <- find_footnote(png2, n.white.parts = 2, n.white.lines = 4)
  
  foot <- max(foot1$foot_line, foot2$foot_line)
  
  png_info <- function(png, foot) {
    list(dim  = dim(png), 
         dpi  = attr(png, "info")$dpi,
         size = dim(png)[1:2] * 2.6 / attr(png, "info")$dpi,
         foot_line = foot$foot_line,
         white_cum = foot$white_cum)
  }
  
  info1 <- png_info(png1, foot = foot1)
  info2 <- png_info(png2, foot = foot2)
  
  diff <- list()
  
  
  # only compare when dimensions fit
  if (all(info1$dim == info2$dim)) {
    diff$identical <- all(png1 == png2)
    
    if (!diff$identical) {
      where_comp <- list(row = range(which(apply(png1 != png2, 1, any)))) 
    } else {
      where_comp <- list(row =  c(nrow(png1), nrow(png1))) 
    }
    
    diff$diff_cm <- round((where_comp$row[2] - where_comp$row[1]) * 2.6 / info1$dpi[1], 2)
    diff$percent <- diff$diff_cm / info1$size[1]
    
    diff$first_dif_after_percent <- 
      100 - round((dim(png1)[1] - where_comp$row[1]) / dim(png1)[1] * 100, 2)
    
    if(any(which(apply(png1[1:foot,,] != png2[1:foot,,], 1, any)))) {
      diff$actual_diff <- TRUE
      
      diff$where <- list(row = range(which(apply(png1[1:foot,,] != png2[1:foot,,], 1, any))),
                         col = range(which(apply(png1[1:foot,,] != png2[1:foot,,], 2, any))))
      
    } else {
      diff$actual_diff <- FALSE 
      
      diff$where <- list(row = c(nrow(png1), nrow(png1)),
                         col = c(ncol(png1), ncol(png1)))
    }
  }
  
  diff$dim_diff <- any(info1$dim != info2$dim)
  diff$dpi_diff <- any(info1$dim != info2$dim)
  
  
  out <- list(info = list(info1 = info1, info2 = info2),
              comp = diff,
              png1 = png1, png2 = png2)
  
  
  class(out) <- "pngcomp"
  out
}


find_footnote <- function(png, n.white.parts = 2, n.white.lines = 4) {
  
  
  # check for footnote using one white line
  white <- apply(png == 1, 1, all)
  
  # search for a box surrounding the png file
  if (all(!white)){
    wh_box_row <- which(apply(png == png[1, 1, ], 1, all))
    wh_box_col <- which(apply(png == png[1, 1, ], 2, all))
    
    white[-wh_box_row] <- apply(png[-wh_box_row, -wh_box_col, ] == 1, 1, all)
  }
  
  # All bottom white before the system footnote is removed 
  add = 0
  if (all(white[(length(white) - n.white.lines + 1):length(white)])) {
    add = 1
  }
  
  a <- matrix(NA, nrow = (length(white)), ncol = n.white.lines)
  
  for (i in 1:(n.white.lines)) {
    a[1:(length(white) - i + 1), i] <- rev(white)[i:length(white)]
  }
  
  white_cum <- apply(a, 1, all)
  white_cum[is.na(white_cum)] <- TRUE  
  
  foot_line <- 
    min(length(white) - which(cumsum((diff(white_cum) == -1)) == 
                                (n.white.parts + add - 1)))[1]
  
  return(list(foot_line = foot_line, white_cum = white_cum))
}

#' print output of pngCompare
#' @param x The comparison to print
#'
#' @param ... Additional arguments passed on, not currently in use.
#' 
#' @rdname pngCompare
#' @export
print.pngcomp <- function(x, ...) {
  
  if (x$comp$dim_diff) {
    message("The plots are of different dimensions and cannot be compared")
    
    cat("\nThe dimensions of file1:\n")
    cat("  ", x$info$info1$dim[1:2], "\n")
    
    cat("\nThe dimensions of file2:\n")
    cat("  ", x$info$info2$dim[1:2], "\n")
    
    return(invisible(x))
  } 
  
  
  if (x$comp$identical) {
    cat(" The plots are completely identical\n")
    
    return(invisible(x))
  }
  
  if (!x$comp$actual_diff) {
    cat(" The plots are identical except for the system footnote\n")
    
    return(invisible(x))
  } else {
    message(" The plots have differences outside the system footnote")
    return(invisible(x))
  }
  
}



#' Summarise a comparison betweeen png files
#'
#' @param object The comparison to summarise 
#' @param ... Additional arguments passed on, not currently in use.
#'
#' @return a summary of the comparison
#' @rdname pngCompare
#' @export
summary.pngcomp <- function(object, ...) {
  
  class(object) <- c("summary.pngcomp", class(object))
  return(object)
}

#' Print a summary of a comparison between png files
#' 
#' @param x The comparison to print
#' @param ... Additional arguments passed on, not currently in use.
#' export
print.summary.pngcomp <- function(x, ...) {
  
  
  print.pngcomp(x) 
  
  if (!x$comp$dim_diff && !x$comp$identical ) {
    cat("\nThe differences covers", x$comp$diff_cm, "cm of the rows\n")
    cat("\nWhich corresponds to", round(x$comp$percent, 1), "% of the graph\n")
    
    cat("\nThe first difference begins after", round(x$comp$first_dif_after_percent, 1), "% of the graph\n")
    
  }
  
  return(invisible(x))
}



#' Plot a comparison of two png files
#'
#' @param x An object created by pngCompare
#' @param ... Additional arguments currently ignored
#' @param base Which of the plots should be considered the base plot, png1
#'   corresponds to file1 and png2 corresponds to file2
#' @param higlight_change Should the changes be hihglighted
#' @param show_foot_line Should the line separating the system footnote from the rest of the graph be shown
#' @param errorcol colour of the error box
#' @param okcol colour of the systemfootnote box
#' @param lwd line width of the box, measured in number of pixels
#' @param debug \code{logical} should a debug plot be shown
#'
#' @return x is returned invisibly.
#' 
#' @export
#' @importFrom grDevices col2rgb as.raster 
#' @importFrom graphics par
plot.pngcomp <- function(x, ..., 
                         base = c("png1", "png2"), 
                         higlight_change = TRUE, 
                         show_foot_line = FALSE,
                         errorcol = 2, okcol = 3, lwd = 4,
                         debug = FALSE) {
  
  base <- match.arg(base)
  
  png1 <- x$png1
  png2 <- x$png2
  
  old_mar <- par()$mar
  par(mar = c(0, 0, 0, 0))
  on.exit(par(mar = old_mar))
  
  png3 <- switch (base,
                  png1 = png1,
                  png2 = png2
  )
  
  info <- switch (base,
                  png1 = x$info$info1,
                  png2 = x$info$info2
  )
  
  
  if (debug) {
    png3[rev(info$white_cum),,2][] <-  0.5    
    
    rgbcol_line <- col2rgb(1, alpha = FALSE)/255
    
    png3[info$foot_line:(min(info$foot_line+1, dim(png3)[1])), ,1:3][] <- rgbcol_line
    
    plot(as.raster(png3))
    
    return(invisible(png3))
  }
  
  
  nrow<- dim(png3)[1]
  ncol<- dim(png3)[2]
  foot <- info$foot_line
  
  # Line marking footnote
  if (show_foot_line) {
    rgbcol_line <- col2rgb(1, alpha = FALSE)/255
    
    png3[foot:(min(foot+1, nrow)), ,1:3][] <- rgbcol_line
  }
  
  if (higlight_change  && !x$comp$identical) {
    
    if (x$comp$dim_diff) {
      message("cannot show the differences as the two plot do not have equal dimensions")
      return(invisible(NULL))
    }
    
    
    # colour of non-error box
    if(any(which(apply(png1[foot:nrow,,] != png2[foot:nrow,,], 1, any)))){
      
      rgbcol_b2 <- col2rgb(okcol, alpha = FALSE)/255
      
      range.row <- foot + range(which(apply(png1[foot:nrow,,] != png2[foot:nrow,,], 1, any))) 
      range.col <- range(which(apply(png1[foot:nrow,,] != png2[foot:nrow,,], 2, any)))
      r.min <- max(range.row[1] - (3 + lwd), 1)
      r.max <- min(range.row[2] + 3, nrow - lwd)
      c.min <- max(range.col[1] - (3 + lwd), 1)
      c.max <- min(range.col[2] + 3, ncol - lwd)
      
      
      for(i in 1:3){
        png3[r.min:(r.min+lwd),
             c.min:(c.max+lwd),i] <- rgbcol_b2[i]
        
        png3[r.max:(r.max+lwd),
             c.min:(c.max+lwd),i] <- rgbcol_b2[i]
        
        png3[r.min:(r.max+lwd),
             c.min:(c.min+lwd),i] <- rgbcol_b2[i]
        
        png3[r.min:(r.max+lwd),
             c.max:(c.max+lwd),i] <- rgbcol_b2[i]
      }
    }
    
    # colour of error box
    if (x$comp$actual_diff) {
      r.min <- max(x$comp$where$row[1] - (3 + lwd), 1)
      r.max <- min(x$comp$where$row[2] + 3, nrow - lwd)
      c.min <- max(x$comp$where$col[1] - (3 + lwd), 1)
      c.max <- min(x$comp$where$col[2] + 3, ncol - lwd)
      
      rgbcol <- col2rgb(errorcol, alpha = FALSE)/255
      
      for(i in 1:3){
        png3[r.min:(r.min+lwd),
             c.min:(c.max+lwd),i] <- rgbcol[i]
        
        png3[r.max:(r.max+lwd),
             c.min:(c.max+lwd),i] <- rgbcol[i]
        
        png3[r.min:(r.max+lwd),
             c.min:(c.min+lwd),i] <- rgbcol[i]
        
        png3[r.min:(r.max+lwd),
             c.max:(c.max+lwd),i] <- rgbcol[i]
        
      }
    }
    
  }
  
  plot(as.raster(png3))
  
  return(invisible(png3))
}


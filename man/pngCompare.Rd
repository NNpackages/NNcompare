% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pngCompare.R
\name{pngCompare}
\alias{pngCompare}
\alias{print.pngcomp}
\alias{summary.pngcomp}
\title{Compare two png files}
\usage{
pngCompare(file1, file2, n.white.parts = 3, n.white.lines = 4)

\method{print}{pngcomp}(x, ...)

\method{summary}{pngcomp}(object, ...)
}
\arguments{
\item{file1}{The path to file1}

\item{file2}{The path to file2}

\item{n.white.parts}{The number of white parts to search for in footnote}

\item{n.white.lines}{The number of lines that constitutes a part}

\item{x}{The comparison to print}

\item{...}{Additional arguments passed on, not currently in use.}

\item{object}{The comparison to summarise}
}
\value{
An object of class

a summary of the comparison
}
\description{
Compare two png files

print output of pngCompare

Summarise a comparison betweeen png files
}
\examples{
# Make sure the training data is setup
NNtraining::createTrainingDB()

# create the paths to two png files that needs to be compared 
output_path = "~/training/nn1234/nn1234-4321/current/stats/output/"
flpageomean.png <- paste0(output_path, "flpageomean.png")
flpageomean_new.png <- paste0(output_path, "flpageomean_new.png")

# Compare the files
pngcompare <- pngCompare(file1 = flpageomean.png, file2 = flpageomean_new.png)

# Did we observe any differences
pngcompare

# where was the differences observed
plot(pngcompare)

}

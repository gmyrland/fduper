% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{remove_files}
\alias{remove_files}
\title{Remove files from fduper object path listing}
\usage{
remove_files(.data, files)
}
\arguments{
\item{.data}{A fduper object}

\item{files}{A vector of file paths}
}
\value{
The modified fduper object with the additional paths removed
}
\description{
Provide a character vector of file paths to remove from the paths in the fduper object.
}
\examples{
f \%>\% remove_files("~/my_file.txt")
}

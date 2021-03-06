% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output.R
\name{to_text_file}
\alias{to_text_file}
\title{Write groups of duplicates to text file}
\usage{
to_text_file(.data, file, overwrite = FALSE)
}
\arguments{
\item{.data}{A fduper object}

\item{file}{The path of the text file to write to}

\item{overwrite}{If TRUE, do not prompt for overwrite of \code{file}, with default as FALSE}
}
\value{
The unmodified fduper object.
}
\description{
Groups the files using the "groups" column, then outputs the resulting
duplicates to a text file. This can be used for examining the duplicates,
or to manually review for duplicate removal, or to feed into some other pipeline.
}
\examples{
f \%>\% to_text_file("myfile.txt", overwrite=TRUE)
}

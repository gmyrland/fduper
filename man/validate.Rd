% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output.R
\name{validate}
\alias{validate}
\title{Validate the duplicates using the "groups" column to identify sets of duplicates.}
\usage{
validate(.data, ...)
}
\arguments{
\item{.data}{A fduper object}

\item{chunk_size}{The size of chunks to read, with default of 1e6 bytes}
}
\value{
The validated fduper object, or stops execution if a set doesn't pass validation
}
\description{
Groups the files using the "groups" column, then performs extra validation
of the groups. In practice, comparing file hashes is probably enough to identify
duplicates for most purposes, but performing the extra validation check can
privde extra peace of mind.
The algorithm works by reading the files in small chunks, hashing the chunks, and
comparing them. Successful validation requires that the hash of each chunk be
identical.
}
\examples{
f \%>\% identify(hash) \%>\% validate(hash)
}

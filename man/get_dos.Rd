% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_properties.R
\name{get_dos}
\alias{get_dos}
\title{Get file hash of dos line-ending version of file}
\usage{
get_dos(path, algo = "md5", default = get_hash(path))
}
\arguments{
\item{path}{The path to a file}

\item{algo}{The hashing algorithm as a string. Default is "md5". See the
digest package for allowable values.}

\item{default}{The value to use for binary files. By default, the file hash
without modification is used. Alternatively, use default=NA to
skip hash generation for binary files.}
}
\value{
The file hash as a string, or default
}
\description{
Returns the hash of the file with line endings converted to dos format.
This is done by creating a temporary file with the alternate endings, hashing
and cleaning up the file.
The purpose is to allow for comparison of dupes where line endings have been changed
between dos and unix formats, making the files have different hashes.
Different hashing algoritms can be used, with the default being md5sum.
Underlying work is performed by the digest package.
The file line ending conversion is performed by unix2dos and requires that
this program is installed. Note that unix2dos identifies binary files and
forgoes the line ending conversion. In this case, a default is used for the
get_dos function.
}

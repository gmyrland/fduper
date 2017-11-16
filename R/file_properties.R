# File Attributes

#' Get filename
#'
#' Returns the basename of a file (the file name without the directory)
#'
#' @param path The path to a file
#' @return The filename without the directory
#' @export
get_filename <- function(path) {
  basename(path)
}

#' Get directory name
#'
#' Returns the directory name of a file
#'
#' @param path The path to a file
#' @return The directory path, without the filename
#' @export
get_directoryname <- function(path) {
  dirname(path)
}

#' Get file extension
#'
#' Returns the file extension of a file
#'
#' @param path The path to a file
#' @return The file extension as a string
#' @export
get_ext <- function(path) {
  sub("^.*[\\\\/][^\\.]+", "", path)
}

#' Get file created date
#'
#' Returns the file created date of a file
#'
#' @param path The path to a file
#' @return The file created date of the file
#' @export
get_created <- function(path) {
  file.info(path)$ctime
}

#' Get file modified date
#'
#' Returns the file modified date of a file
#'
#' @param path The path to a file
#' @return The file modified date of the file
#' @export
get_modified <- function(path) {
  file.info(path)$mtime
}

#' Get file size in bytes
#'
#' Returns the file size date of a file in bytes
#'
#' @param path The path to a file
#' @return The file size of the file in bytes
#' @export
get_size <- function(path) {
  file.info(path)$size
}

#' Get file hash
#'
#' Returns the hash of the file. Different hashing algoritms can be used,
#' with the default being md5sum. Underlying work is performed by the
#' digest package.
#'
#' @param path The path to a file
#' @param algo The hashing algorithm as a string. Default is "md5". See the
#'             digest package for allowable values.
#' @return The file hash as a string
#' @export
get_hash <- Vectorize(function(path, algo="md5") {
  if (!file.exists(path)) return(NA)
  digest::digest(file=path, algo=algo)
}, "path")

#' Get file hash of dos line-ending version of file
#'
#' Returns the hash of the file with line endings converted to dos format.
#' This is done by creating a temporary file with the alternate endings, hashing
#' and cleaning up the file.
#' The purpose is to allow for comparison of dupes where line endings have been changed
#' between dos and unix formats, making the files have different hashes.
#' Different hashing algoritms can be used, with the default being md5sum.
#' Underlying work is performed by the digest package.
#' The file line ending conversion is performed by unix2dos and requires that
#' this program is installed. Note that unix2dos identifies binary files and
#' forgoes the line ending conversion. In this case, a default is used for the
#' get_dos function.
#'
#' @param path The path to a file
#' @param algo The hashing algorithm as a string. Default is "md5". See the
#'             digest package for allowable values.
#' @param default The value to use for binary files. By default, the file hash
#'                without modification is used. Alternatively, use default=NA to
#'                skip hash generation for binary files.
#' @return The file hash as a string, or default
#' @export
get_dos <- Vectorize(function(path, algo="md5", default=get_hash(path)) {
  if (!file.exists(path)) return(NA)
  t <- tempfile()
  on.exit(unlink(t))
  file.copy(path, t)
  res <- system2("unix2dos", t, stderr=TRUE, stdout=TRUE)
  if(grepl("unix2dos: Binary", res[1])) return(default)
  get_hash(t, algo)
}, c("path", "algo", "default"))

#' Get file hash of unix line-ending version of file
#'
#' Returns the hash of the file with line endings converted to unix format.
#' This is done by creating a temporary file with the alternate endings, hashing
#' and cleaning up the file.
#' The purpose is to allow for comparison of dupes where line endings have been changed
#' between dos and unix formats, making the files have different hashes.
#' Different hashing algoritms can be used, with the default being md5sum.
#' Underlying work is performed by the digest package.
#' The file line ending conversion is performed by unix2dos and requires that
#' this program is installed. Note that unix2dos identifies binary files and
#' forgoes the line ending conversion. In this case, a default is used for the
#' get_unix function.
#'
#' @param path The path to a file
#' @param algo The hashing algorithm as a string. Default is "md5". See the
#'             digest package for allowable values.
#' @param default The value to use for binary files. By default, the file hash
#'                without modification is used. Alternatively, use default=NA to
#'                skip hash generation for binary files.
#' @return The file hash as a string, or default
#' @export
get_unix <- Vectorize(function(path, algo="md5", default=get_hash(path)) {
  if (!file.exists(path)) return(NA)
  t <- tempfile()
  on.exit(unlink(t))
  file.copy(path, t)
  res <- system2("dos2unix", t, stderr=TRUE, stdout=TRUE)
  if(grepl("dos2unix: Binary", res[1])) return(default)
  get_hash(t, algo)
}, c("path", "algo", "default"))


## Quick Adders

#' Add filename
#'
#' Adds or overwrites a column named "filename", containing the filename (without
#' directory) of the path for each file.
#'
#' @param .data A fduper object
#' @return The modified fduper object with filenames
#' @seealso \code{\link{get_filename}}
#' @export
add_filename <- function(.data) {
  UseMethod("add_filename")
}
add_filename.fduper <- function(.data) {
  mutate(.data, filename = get_filename(path))
}

#' Add directory name
#'
#' Adds or overwrites a column named "directoryname", containing the directory
#' of each file.
#'
#' @param .data A fduper object
#' @return The modified fduper object with directory names
#' @seealso \code{\link{get_directoryname}}
#' @export
add_directoryname <- function(.data) {
  UseMethod("add_directoryname")
}
#' @export
add_directoryname.fduper <- function(.data) {
  mutate(.data, directoryname = get_directoryname(path))
}

#' Add file extension
#'
#' Adds or overwrites a column named "ext", containing the file extension
#' of each file.
#'
#' @param .data A fduper object
#' @return The modified fduper object with file extensions
#' @seealso \code{\link{get_ext}}
#' @export
add_ext <- function(.data) {
  UseMethod("add_ext")
}
#' @export
add_ext.fduper <- function(.data) {
  mutate(.data, ext = get_ext(path))
}

#' Add file created date
#'
#' Adds or overwrites a column named "created", containing the file created date
#' of each file.
#'
#' @param .data A fduper object
#' @return The modified fduper object with file created dates
#' @seealso \code{\link{get_created}}
#' @export
add_created <- function(.data) {
  UseMethod("add_created")
}
#' @export
add_created.fduper <- function(.data) {
  mutate(.data, created = get_created(path))
}

#' Add file modified date
#'
#' Adds or overwrites a column named "modified", containing the file modification date
#' of each file.
#'
#' @param .data A fduper object
#' @return The modified fduper object with file modified dates
#' @seealso \code{\link{get_modified}}
#' @export
add_modified <- function(.data) {
  UseMethod("add_modified")
}
#' @export
add_modified.fduper <- function(.data) {
  mutate(.data, modified = get_modified(path))
}

#' Add file size in bytes
#'
#' Adds or overwrites a column named "size", containing the file size in bytes
#' for each file.
#'
#' @param .data A fduper object
#' @return The modified fduper object with file sizes in bytes
#' @seealso \code{\link{get_size}}
#' @export
add_size <- function(.data) {
  UseMethod("add_size")
}
#' @export
add_size.fduper <- function(.data) {
  mutate(.data, size = get_size(path))
}

#' Add file hash
#'
#' Adds or overwrites a column named "hash", containing the file hash
#' for each file, as computed by the \code{get_hash} function.
#'
#' @param .data A fduper object
#' @return The modified fduper object with file hashes
#' @seealso \code{\link{get_hash}}
#' @export
add_hash <- function(.data, ...) {
  UseMethod("add_hash")
}
#' @export
add_hash.fduper <- function(.data, ...) {
  mutate(.data, hash = get_hash(path, ...))
}

#' Add file hash for dos line-endings
#'
#' Adds or overwrites a column named "dos", containing the file hash
#' for each file, as computed by the \code{get_dos} function.
#'
#' @param .data A fduper object
#' @return The modified fduper object with dos file hashes
#' @seealso \code{\link{get_dos}}
#' @export
add_dos <- function(.data, ...) {
  UseMethod("add_dos")
}
#' @export
add_dos.fduper <- function(.data, ...) {
  mutate(.data, dos = get_dos(path, ...))
}

#' Add file hash for unix line-endings
#'
#' Adds or overwrites a column named "unix", containing the file hash
#' for each file, as computed by the \code{get_unix} function.
#'
#' @param .data A fduper object
#' @return The modified fduper object with unix file hashes
#' @seealso \code{\link{get_unix}}
#' @export
add_unix <- function(.data, ...) {
  UseMethod("add_unix")
}
#' @export
add_unix.fduper <- function(.data, ...) {
  mutate(.data, unix  = get_unix(path, ...))
}

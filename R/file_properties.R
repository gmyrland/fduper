# File Attributes

#' Get filename
#' @export
get_filename <- function(path) {
  basename(path)
}

#' Get directory name
#' @export
get_directoryname <- function(path) {
  dirname(path)
}

#' Get file extension
#' @export
get_ext <- function(path) {
  sub("^.*\\.", "", path)
}

#' Get file created date
#' @export
get_created <- function(path) {
  file.info(path)$ctime
}

#' Get file modified date
#' @export
get_modified <- function(path) {
  file.info(path)$mtime
}

#' Get file size in bytes
#' @export
get_size <- function(path) {
  file.info(path)$size
}

#' Get file hash using various algorithms
#' @export
get_hash_ <- function(path, algo="md5") {
  if (!file.exists(path)) return(NA)
  digest::digest(file=path, algo=algo)
}
get_hash <- Vectorize(get_hash_, "path")

#' Get file hash of dos line-ending version of file
#' @export
get_dos_ <- function(path, algo="md5", default=get_hash(path)) {
  if (!file.exists(path)) return(NA)
  t <- tempfile()
  on.exit(unlink(t))
  file.copy(path, t)
  res <- system2("unix2dos", t, stderr=TRUE, stdout=TRUE)
  if(grepl("unix2dos: Binary", res[1])) return(default)
  get_hash_(t, algo)
}
get_dos <- Vectorize(get_dos_, c("path", "algo", "default"))

#' Get file hash of unix line-ending version of file
#' @export
get_unix_ <- function(path, algo="md5", default=get_hash(path)) {
  if (!file.exists(path)) return(NA)
  t <- tempfile()
  on.exit(unlink(t))
  file.copy(path, t)
  res <- system2("dos2unix", t, stderr=TRUE, stdout=TRUE)
  if(grepl("dos2unix: Binary", res[1])) return(default)
  get_hash_(t, algo)
}
get_unix <- Vectorize(get_unix_, c("path", "algo", "default"))


# Quick Adders

#' Add filename
#' @export
add_filename <- function(.data) {
  UseMethod("add_filename")
}
add_filename.fduper <- function(.data) {
  mutate(.data, filename = get_filename(path))
}

#' Add directory name
#' @export
add_directoryname <- function(.data) {
  UseMethod("add_directoryname")
}
#' @export
add_directoryname.fduper <- function(.data) {
  mutate(.data, directoryname = get_directoryname(path))
}

#' Add file extension
#' @export
add_ext <- function(.data) {
  UseMethod("add_ext")
}
#' @export
add_ext.fduper <- function(.data) {
  mutate(.data, ext = get_ext(path))
}

#' Add file created date
#' @export
add_created <- function(.data) {
  UseMethod("add_created")
}
#' @export
add_created.fduper <- function(.data) {
  mutate(.data, created = get_created(path))
}

#' Add file modified date
#' @export
add_modified <- function(.data) {
  UseMethod("add_modified")
}
#' @export
add_modified.fduper <- function(.data) {
  mutate(.data, modified = get_modified(path))
}

#' Add file size in bytes
#' @export
add_size <- function(.data) {
  UseMethod("add_size")
}
#' @export
add_size.fduper <- function(.data) {
  mutate(.data, size = get_size(path))
}

#' Add file hash
#' @export
add_hash <- function(.data, ...) {
  UseMethod("add_hash")
}
#' @export
add_hash.fduper <- function(.data, ...) {
  mutate(.data, hash = get_hash(path, ...))
}

#' Add file hash for dos line-endings
#' @export
add_dos <- function(.data, ...) {
  UseMethod("add_dos")
}
#' @export
add_dos.fduper <- function(.data, ...) {
  mutate(.data, dos = get_dos(path, ...))
}

#' Add file hash for unix line-endings
#' @export
add_unix <- function(.data, ...) {
  UseMethod("add_unix")
}
#' @export
add_unix.fduper <- function(.data, ...) {
  mutate(.data, unix  = get_unix(path, ...))
}

# File Attributes

get_filename <- function(f) {
  basename(f)
}

get_directoryname <- function(f) {
  dirname(f)
}

get_ext <- function(f) {
  sub("^.*\\.", "", f)
}

get_created <- function(f) {
  file.info(f)$ctime
}

get_modified <- function(f) {
  file.info(f)$mtime
}

get_size <- function(f) {
  file.info(f)$size
}

get_hash_ <- function(f, algo="md5") {
  cat(f, "\n")
  if (!file.exists(f)) return(NA)
  digest::digest(file=f, algo=algo)
}

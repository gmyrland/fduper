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
get_hash <- Vectorize(get_hash_, "f")

get_dos_ <- function(f, algo="md5", default=get_hash(f)) {
  if (!file.exists(f)) return(NA)
  t <- tempfile()
  on.exit(unlink(t))
  file.copy(f, t)
  res <- system2("unix2dos", t, stderr=TRUE, stdout=TRUE)
  if(grepl("unix2dos: Binary", res[1])) return(default)
  get_hash_(t, algo)
}
get_dos <- Vectorize(get_dos_, c("f", "algo", "default"))

get_unix_ <- function(f, algo="md5", default=get_hash(f)) {
  if (!file.exists(f)) return(NA)
  t <- tempfile()
  on.exit(unlink(t))
  file.copy(f, t)
  res <- system2("dos2unix", t, stderr=TRUE, stdout=TRUE)
  if(grepl("dos2unix: Binary", res[1])) return(default)
  get_hash_(t, algo)
}
get_unix <- Vectorize(get_unix_, c("f", "algo", "default"))

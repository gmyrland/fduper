# Types of methods to include:

# # file selection
# add_files
# remove_files
# add_dir
# remove_files_by_pattern

# # property generation
# mutate

# # reduction
# select
# filter
# reduce_by
# reduce_by_n
# must_contain
# must_not_contain

# # handling
# to_csv
# to...
# interactive_delete

################
# File Selection

#' add_files: Add character vector of files to fduper object
#' @export
add_files <- function(.data, files) {
  UseMethod("add_files")
}
#' @export
add_files.fduper <- function(.data, files) {
  paths <- unique(normalizePath(files))
  paths <- paths[file.info(paths)$isdir == FALSE] # should warn about this
  new_paths <- setdiff(paths, .data$path)
  dplyr::bind_rows(.data, fduper(path=new_paths))
}

#' add_dirs: Add files in directories to fduper object
#' @export
add_dirs <- function(.data, path, recursive=TRUE, ...) {
  UseMethod("add_dirs")
}
#' @export
add_dirs.fduper <- function(.data, path, ...) {
  paths <- unique(get_files(path, ...))
  new_paths <- setdiff(paths, .data$path)
  dplyr::bind_rows(.data, fduper(path=new_paths))
}

#' remove_files: Remove character vector of files from fduper object
#' @export
remove_files <- function(.data, files) {
  UseMethod("remove_files")
}
#' @export
remove_files.fduper <- function(.data, files) {
  paths <- unique(normalizePath(files))
  dirs <- paths[file.info(paths)$isdir]
  paths <- paths[!paths %in% dirs]
  sapply(dirs, function(dir) warning("Ignoring Directory: ", dir))
  filter(.data, !path %in% paths)
}

#' remove_files_by_pattern: Remove files by matching regex
#' @export
remove_files_by_pattern <- function(.data, files) {
  UseMethod("remove_files_by_pattern")
}
#' @export
remove_files_by_pattern.fduper <- function(.data, pattern) {
  filter(.data, !grepl(pattern, path))
}

#' keep_files_by_pattern: Keep files by matching regex
#' @export
keep_files_by_pattern <- function(.data, files) {
  UseMethod("keep_files_by_pattern")
}
#' @export
keep_files_by_pattern.fduper <- function(.data, pattern) {
  filter(.data, grepl(pattern, path))
}

#' remove_dir: Remove files in directories from fduper object
#' @export
remove_dirs <- function(.data, path, ...) {
  UseMethod("remove_dirs")
}
#' @export
remove_dirs.fduper <- function(.data, dir, ...) {
  paths <- get_files(dir, ...)
  remove_files(.data, paths)
}

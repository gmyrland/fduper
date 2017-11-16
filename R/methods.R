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

#' Add files to fduper object path listing
#'
#' Provide a character vector of file paths to add to the paths in the fduper object.
#'
#' @param .data A fduper object
#' @param files A vector of file paths
#' @return The modified fduper object with the additional paths added
#' @examples
#' f %>% add_files("~/my_file.txt")
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

#' Add files in directories to fduper object path listing
#'
#' Provide a character vector of directory paths. The directory paths are searched
#' and the resulting files are added to the paths in the fduper object.
#'
#' @param .data A fduper object
#' @param path A vector of directory paths
#' @param recursive A logical indicating whether or not to search the directories
#'                  recursively, with default of TRUE
#' @param ... Additional arguments to pass to the \code{get_files} command
#' @return The modified fduper object with the additional paths added
#' @examples
#' f %>% add_dirs("~/my_directory")
#' @seealso \code{\link{get_files}}
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

#' Remove files from fduper object path listing
#'
#' Provide a character vector of file paths to remove from the paths in the fduper object.
#'
#' @param .data A fduper object
#' @param files A vector of file paths
#' @return The modified fduper object with the additional paths removed
#' @examples
#' f %>% remove_files("~/my_file.txt")
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

#' Remove files from fduper object path listing matching the provided regex
#'
#' Uses the provided regex pattern to compare the paths in the fduper object, and
#' removes the files matching the regex pattern.
#'
#' @param .data A fduper object
#' @param pattern A regex pattern to check existing paths in fduper object against
#' @return The modified fduper object with the matching paths removed
#' @examples
#' f %>% remove_files_by_pattern("\\.txt$")
#' @export
remove_files_by_pattern <- function(.data, pattern) {
  UseMethod("remove_files_by_pattern")
}
#' @export
remove_files_by_pattern.fduper <- function(.data, pattern) {
  filter(.data, !grepl(pattern, path))
}

#' Keep files from fduper object path listing matching the provided regex
#'
#' Uses the provided regex pattern to compare the paths in the fduper object, and
#' removes the files not matching the regex pattern.
#'
#' @param .data A fduper object
#' @param files A vector of file paths
#' @return The modified fduper object with non-matching paths removed
#' @examples
#' f %>% keep_files_by_pattern("\\.txt$")
#' @export
keep_files_by_pattern <- function(.data, files) {
  UseMethod("keep_files_by_pattern")
}
#' @export
keep_files_by_pattern.fduper <- function(.data, pattern) {
  filter(.data, grepl(pattern, path))
}

#' Remove files in directories to fduper object path listing
#'
#' Provide a character vector of directory paths. The directory paths are searched
#' and the resulting files are removed from the paths in the fduper object.
#'
#' @param .data A fduper object
#' @param path A vector of directory paths
#' @param recursive A logical indicating whether or not to search the directories
#'                  recursively, with default of TRUE
#' @param ... Additional arguments to pass to the \code{get_files} command
#' @return The modified fduper object with the additional paths removed
#' @examples
#' f %>% remove_dirs("~/my_directory")
#' @seealso \code{\link{get_files}}
#' @export
remove_dirs <- function(.data, path, recursive=TRUE, ...) {
  UseMethod("remove_dirs")
}
#' @export
remove_dirs.fduper <- function(.data, dir, ...) {
  paths <- get_files(dir, ...)
  remove_files(.data, paths)
}

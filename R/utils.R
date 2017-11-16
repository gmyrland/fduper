# utils

#' Collect file paths
#'
#' The \code{get_files} function addresses some of the quirks with list.files
#' and uses defaults that are more useful for fduper, such as \code{recursive = TRUE}
#'
#' @param path A character vector of path names to search
#' @param pattern An optional regex pattern to match results against
#' @param all.files Whether or not to include files starting with period, with default as TRUE.
#' @param recursive Whether or not to search path recursively, with default as TRUE
#' @param ignore.case Whether or not to use case-sensitive pattern matching, with default as FALSE
#' @return A character vector containing the resulting file paths
#' @examples
#' get_files(sample_path(), recursive=TRUE)
#' get_files(sample_path(), recursive=FALSE)
#' get_files(sample_path(), "*.txt$")
#' @export
get_files <- function(path, pattern = NULL, all.files = TRUE,
                      recursive = TRUE, ignore.case = FALSE) {
  paths <- list.files(path, pattern = pattern, recursive = recursive,
                      all.files = all.files, ignore.case = ignore.case,
                      full.names = TRUE, include.dirs = FALSE, no.. = TRUE)
  if (!isTRUE(recursive)) {
    paths <- setdiff(paths, list.dirs(path, recursive = recursive, full.names = TRUE))
  }
  (paths)
}

#' Collect directory paths
#'
#' The \code{get_dirs} function addresses some of the quirks with list.files
#' and uses defaults that are more useful for fduper, such as \code{recursive = TRUE}
#'
#' @param path A character vector of path names to search
#' @param pattern An optional regex pattern to match results against
#' @param all.dirs Whether or not to include directories starting with period, with default as TRUE.
#' @param recursive Whether or not to search path recursively, with default as TRUE
#' @param ignore.case Whether or not to use case-sensitive pattern matching, with default as FALSE
#' @return A character vector containing the resulting directory paths
#' @examples
#' get_dirs(sample_path(), recursive=TRUE)
#' get_dirs(sample_path(), recursive=FALSE)
#' @export
get_dirs <- function(path, pattern=NULL, all.dirs = TRUE, recursive = TRUE, ignore.case = FALSE) {
  # pattern should apply to directory, not the files probably?
  paths <- list.files(path, pattern = pattern, recursive = recursive,
                      all.files = all.dirs, ignore.case = ignore.case,
                      full.names = TRUE, include.dirs = TRUE, no.. = TRUE
                      )
  paths <- paths[file.info(paths)$isdir]
  (paths)
}

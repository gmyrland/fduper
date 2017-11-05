# utils

#' get_files: Collect file paths
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

#' get_dirs: Collect directory paths
#' @examples
#' get_dirs(sample_path(), recursive=TRUE)
#' get_dirs(sample_path(), recursive=FALSE)
#' @export
get_dirs <- function(path, pattern=NULL, all.dirs = TRUE, recursive = TRUE, ignore.case = FALSE) {
  paths <- list.files(path, pattern = pattern, recursive = recursive,
                      all.files = all.dirs, ignore.case = ignore.case,
                      full.names = TRUE, include.dirs = TRUE, no.. = TRUE
                      )
  paths <- paths[file.info(paths)$isdir]
  (paths)
}

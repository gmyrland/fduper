# utils

# Collect file paths
get_files <- function(path, pattern = NULL, all.files = TRUE, recursive = TRUE, ...) {
  paths <- list.files(path, pattern = pattern, recursive = recursive,
                      all.files = all.files, full.names = TRUE, no.. = TRUE, ...)
  if (!isTRUE(recursive)) {
    paths <- setdiff(paths, list.dirs(path, recursive = recursive, full.names = TRUE))
  }
  (paths)
}
#get_files("~/data", recursive=TRUE)
#get_files("~/data", recursive=FALSE)
#get_files("~/data", "*.bz2$", recursive=TRUE)

get_dirs <- function(path, pattern=NULL, all.files = TRUE, recursive = TRUE, ...) {
  paths <- list.files(path, pattern = pattern, recursive = recursive,
                      all.files = all.files, full.names = TRUE, no.. = TRUE,
                      include.dirs = TRUE, ...)
  paths <- paths[file.info(paths)$isdir]
  (paths)
}
#get_dirs("~/data", recursive=TRUE)
#get_dirs("~/data", recursive=FALSE)
#get_dirs("~/source", "\\.git$", recursive=TRUE)
#get_files(get_dirs("~/source", "\\.git$")) # for example, to remove from file list

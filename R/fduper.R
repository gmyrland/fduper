#' Eliminate Duplicate Files
#'
#' fduper allows you to create custom workflows for removing duplicate files.
#' There are many existing file deduplicators, however, they typically don't
#' allow for nuanced file deduping strategies. While they typically provide a
#' means to add and ignore folders recursively, it tends to be difficult to cut
#' through the noise of legitimate dupes to focus on the real ones. Additionally,
#' casting a wide net by searching a large number of files to find all duplicates
#' can be computationally intensive. If the task is simply check a handful of
#' orphaned files in a folder, but their duplicates may be anywhere on the drive,
#' it can lead to significant time wasted computing hashes of duplicate pairs that
#' do not inlcude the orphaned files in quesion.
#'
#' A solution to this is to increase the flexibility and availability of the
#' constratints in the deduplication process. For example, it may be nice to do a
#' a workflow such as "search entire drive for all image files, but only flag
#' duplicate pairs if at least one is in the ~/images. In that case, delete the
#' other file, but not if it's in a path that contains a .git folder because that is
#' likely to be a legitimate duplicate."
#'
#' At its heart, fduper extends dplyr to include methods relevant to the file
#' deduplication process. The underlying data object is a tibble of arbitrary
#' file information (path, size, hashes, etc.). Since this structure will be
#' familiar to any R user, it should be relatively easy to add custom steps to
#' further extend fduper for your custom workflow. The intent of fduper is not
#' to be the fastest deduper, but to provide a readily understandable and
#' hackable user experience. And in practice, a well crafted workflow can often
#' save substantial unnecessary computation by file dedupers that don't allow
#' for more finesse in their process.
#'
#' fduper is under development and should be used with caution.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr tibble
"_PACKAGE"

#' fduper constructor
#'
#' Create a fduper object to be passed to subsequent methods from the fduper
#' package.
#'
#' @param path A character vector of paths to be added recursively
#' @param ... Additional columns to add to the underlying fduper tibble
#' @return A fduper object, to be piped to other methods for processing.
#' @examples
#' fduper()
#' @export
fduper <- function(path=character(0), ...) {
  if (length(path) == 1 && file.info(path)$isdir)
    path <- get_files(path)
  .data <- tibble(path, ...)
  class(.data) <- c("fduper", class(.data))
  (.data)
}

#' Determine if object is class fduper
#'
#' Check if object contains the class "fduper".
#'
#' @param .data A object to be evaluated
#' @return TRUE if object is of class fduper
#' @export
is_fduper <- function(.data) inherits(.data, "fduper")

#' Add fduper to object class list
#'
#' Coerce to fduper object. Note that this function does not check for
#' compatibility of the underlying object so use with care. In general,
#' you would use this to convert a tibble or data frame to a fduper object
#' to use the fduper S3 methods. Note that many of these methods expect
#' a "path" column to exist and contain full file paths.
#'
#' @param .data An object to be given class fduper
#' @return The modified object as class fduper
#' @export
as_fduper <- function(.data) {
  class(.data) <- c("fduper", setdiff(class(.data), "fduper"))
  (.data)
}

#' Print fduper object
#'
#' To provides a custom print method for fduper object.
#' At the moment, this simply wraps the next print method.
#'
#' @param .data A fduper object to be printed
#' @return The unmodified .data object
#' @export
print.fduper <- function(.data, ...) {
  as_fduper(NextMethod("print", .data))
}

#' Sample documents path for testing
#'
#' For testing workflows, this sample document path can be used
#'
#' @return A path to some test files within the package directory
#' @export
sample_path <- function() {
  system.file(package="fduper", "sample")
}

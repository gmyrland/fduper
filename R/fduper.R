#' fduper: Eliminate Duplicate Files
#'
#' @import dplyr
#' @export %>%
## @exportPattern ^[\.]
"fduper"

#' fduper: Constructor
#'
#' @param path A character vector of paths
#' @param ... Additional columns to add to the tibble
#' @return A fduper object
#' @export
fduper <- function(path=character(0), ...) {
  if (length(path) == 1 && file.info(path)$isdir)
    path <- get_files(path)
  .data <- tibble(path, ...)
  class(.data) <- c("fduper", class(.data))
  (.data)
}

#' is_fduper: Check if object is class fduper
#' @export
is_fduper <- function(.data) inherits(.data, "fduper")

#' as_fduper: Add fduper to object class
#' @export
as_fduper <- function(.data) {
  class(.data) <- c("fduper", setdiff(class(.data), "fduper"))
  (.data)
}

#' print: Print fduper object
#' @export
print.fduper <- function(.data, ...) {
  as_fduper(NextMethod("print", .data))
}

#' sample_path: Sample documents for testing
#' @export
sample_path <- function() {
  system.file(package="fduper", "sample")
}

#' fduper: Eliminate Duplicate Files
#'
#' @import dplyr
#' @export %>%
"fduper"

#' fduper: Constructor
#'
#' @param path A character vector of paths
#' @param ... Additional columns to add to the tibble
#' @return A fduper object
#' @export
fduper <- function(path=character(0), ...) {
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

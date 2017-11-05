# fduper constructor
fduper <- function(path=character(0), ...) {
  .data <- tibble(path, ....)
  class(.data) <- c("fduper", class(.data))
  (.data)
}

is_fduper <- function(.data) inherits(.data, "fduper")

as_fduper <- function(.data) {
  class(.data) <- c("fduper", setdiff(class(.data), "fduper"))
  (.data)
}

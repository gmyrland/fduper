# Property Modifiers, etc.

#' fduper wrapper for dplyr mutate
#' @importFrom dplyr mutate
#' @export
mutate.fduper <- function(.data, ...) {
  as_fduper(NextMethod("mutate", .data))
}

#' fduper wrapper for dplyr arrange
#' @importFrom dplyr arrange
#' @export
arrange.fduper <- function(.data, ...) {
  as_fduper(NextMethod("arrange", .data, ...))
}

#' fduper wrapper for dplyr filter
#' @importFrom dplyr filter
#' @export
filter.fduper <- function(.data, ...) {
  as_fduper(NextMethod("filter", .data, ...))
}

#' fduper wrapper for dplyr group_by
#' @importFrom dplyr group_by
#' @export
group_by.fduper <- function(.data, ...) {
  as_fduper(NextMethod("group_by", .data))
}

#' fduper wrapper for dplyr ungroup
#' @importFrom dplyr ungroup
#' @export
ungroup.fduper <- function(.data, ...) {
  as_fduper(NextMethod("ungroup", .data))
}


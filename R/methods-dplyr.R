# Property Modifiers, etc.

#' fduper wrapper for dplyr mutate
#' @importFrom dplyr mutate
#' @param .data A fduper object
#' @return The fduper object after processing
#' @export
mutate.fduper <- function(.data, ...) {
  as_fduper(NextMethod("mutate", .data))
}

#' fduper wrapper for dplyr arrange
#' @importFrom dplyr arrange
#' @param .data A fduper object
#' @return The fduper object after processing
#' @export
arrange.fduper <- function(.data, ...) {
  as_fduper(NextMethod("arrange", .data, ...))
}

#' fduper wrapper for dplyr select
#' @importFrom dplyr select
#' @param .data A fduper object
#' @return The fduper object after processing
#' @export
select.fduper <- function(.data, ...) {
  as_fduper(NextMethod("select", .data, ...))
}

#' fduper wrapper for dplyr filter
#' @importFrom dplyr filter
#' @param .data A fduper object
#' @return The fduper object after processing
#' @export
filter.fduper <- function(.data, ...) {
  as_fduper(NextMethod("filter", .data, ...))
}

#' fduper wrapper for dplyr group_by
#' @importFrom dplyr group_by
#' @param .data A fduper object
#' @return The fduper object after processing
#' @export
group_by.fduper <- function(.data, ...) {
  as_fduper(NextMethod("group_by", .data))
}

#' fduper wrapper for dplyr ungroup
#' @importFrom dplyr ungroup
#' @param .data A fduper object
#' @return The fduper object after processing
#' @export
ungroup.fduper <- function(.data, ...) {
  as_fduper(NextMethod("ungroup", .data))
}

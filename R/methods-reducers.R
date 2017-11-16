# Dataset Reducers
# These could use a better name, but essentially they are functions that
# eliminate the non-duplicates from the dataset.

#' Reduce fduper data to files which share certain values with other files.
#'
#' Groups the files in the fduper dataset by the columns pass in the dot arguments,
#' then eliminates groups that do not contain two or more files.
#'
#' @param .data A fduper object
#' @param ... Column names for grouping files as duplicates or potential duplicates
#' @return The modified fduper object with non-duplicates removed
#' @examples
#' f %>% reduce(size)
#' f %>% reduce(size, hash)
#' @export
reduce <- function(.data, ...) {
  UseMethod("reduce")
}
#' @export
reduce.fduper <- function(.data, ...) {
  .data %>%
    group_by(...) %>%
    filter(n() >= 2) %>%
    ungroup
}

#' Reduce fduper data to files which share certain values with other files.
#'
#' Groups the files in the fduper dataset by the columns pass in the dot arguments,
#' then eliminates groups that do not contain two or more files. The \code{reduce_n} variant
#' allows for a mininum and maximum group size specification, where the defaults
#' behave the same as the regular \code{reduce} function.
#'
#' @param .data A fduper object
#' @param .n The minimum group size, with a default of 2
#' @param .n_max The maximum group size, with a default of Inf
#' @param ... Column names for grouping files as duplicates or potential duplicates
#' @return The modified fduper object with non-duplicates removed
#' @examples
#' f %>% reduce_n(size, .n=5)
#' @export
reduce_n <- function(.data, ...) {
  UseMethod("reduce_n")
}
#' @export
reduce_n.fduper <- function(.data, .n = 2, .n_max = Inf, ...) {
  .data %>%
    group_by(...) %>%
    filter(n() >= .n, n() <= .n_max) %>%
    ungroup
}

#' Label groups of items sharing certain values
#'
#' Groups the files in the fduper dataset by the columns pass in the dot arguments,
#' then adds a column "group" with a unique value for each set of duplicates.
#'
#' @param .data A fduper object
#' @param ... Column names for grouping files as duplicates
#' @return The modified fduper object with duplicate groups labelled
#' @examples
#' f %>% identify(size)
#' f %>% identify(size, hash)
#' @importFrom dplyr group_indices
#' @export
identify <- function(.data, ...) {
  UseMethod("identify")
}
#' @export
identify.fduper <- function(.data, ...) {
  .data$group <- NULL
  .data$group <- group_indices(.data, ...)
  (.data)
}

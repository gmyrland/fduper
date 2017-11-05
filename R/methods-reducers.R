# Dataset Reducers

#' Reduce fduper data to items sharing certain values
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

#' Reduce fduper data to items sharing certain values
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


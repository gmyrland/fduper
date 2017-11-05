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
    filter(n()>1) %>%
    ungroup
}

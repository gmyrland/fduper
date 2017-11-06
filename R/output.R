#' Write groups file
#' @export
to_text_file <- function(.data, file, overwrite=FALSE) {
  UseMethod("to_text_file")
}
to_text_file.fduper <- function(.data, file, overwrite=FALSE) {
  if (file.exists(file)) {
    if (!isTRUE(overwrite)) {
      resp <- readline(paste0("File ", file, " exists! Overwrite? (y/n)"))
      if (!tolower(resp) %in% c("y", "yes"))
        stop("File write cancelled")
    }
  }
  if (!"group" %in% names(.data)) stop("Use identify(<features>) to apply group ids")
  file.remove(file)
  .data %>%
    group_by(group) %>%
    do({
      write(c(.$path, ""), file, append = TRUE)
      .
    })
  (.data)
}

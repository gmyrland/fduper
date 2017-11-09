#' Validate
#' @export
validate <- function(.data, ...) {
  UseMethod("validate")
}
validate.fduper <- function(.data, chunk_size = 1e6) {
  read_chunk <- function(con, chunk_size) {
    readBin(con, integer(), n = chunk_size, size=1)
  }

  validate_group <- function(paths) {
    result <- TRUE
    size <- unique(file.info(paths)$size)
    if (length(size) > 1)
      return(false)
    if (size == 0)
      return(TRUE)
    con <- lapply(paths, file, "rb")
    for (i in seq(from = 0, to = size - 1, by = chunk_size)) {
      chunks <- lapply(con, read_chunk, chunk_size)
      cat("Chunks: ", sapply(chunks, digest::digest), sep = "  ", "[", i * chunk_size, "]", "\n")
      if (!length(unique(chunks)) == 1) {
        result <- FALSE; break
      }
    }
    lapply(con, close)
    (result)
  }

  if (!"group" %in% names(.data)) stop("Use identify(<features>) to apply group ids")
  .data %>%
    group_by(group) %>%
    dplyr::do({
      if (!validate_group(.$path)) {
        stop("Non-duplicates found")
        return(filter(.data, FALSE))
      }
      .data
    }) %>%
    ungroup %>% as_fduper
}


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
    file.remove(file)
  }
  if (!"group" %in% names(.data)) stop("Use identify(<features>) to apply group ids")
  .data %>%
    group_by(group) %>%
    dplyr::do({
      write(c(.$path, ""), file, append = TRUE)
      .
    })
  (.data)
}

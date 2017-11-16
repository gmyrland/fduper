#' Validate the duplicates using the "groups" column to identify sets of duplicates.
#'
#' Groups the files using the "groups" column, then performs extra validation
#' of the groups. In practice, comparing file hashes is probably enough to identify
#' duplicates for most purposes, but performing the extra validation check can
#' privde extra peace of mind.
#' The algorithm works by reading the files in small chunks, hashing the chunks, and
#' comparing them. Successful validation requires that the hash of each chunk be
#' identical.
#'
#' @param .data A fduper object
#' @param chunk_size The size of chunks to read, with default of 1e6 bytes
#' @return The validated fduper object, or stops execution if a set doesn't pass validation
#' @examples
#' f %>% identify(hash) %>% validate(hash)
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


#' Write groups of duplicates to text file
#'
#' Groups the files using the "groups" column, then outputs the resulting
#' duplicates to a text file. This can be used for examining the duplicates,
#' or to manually review for duplicate removal, or to feed into some other pipeline.
#'
#' @param .data A fduper object
#' @param file The path of the text file to write to
#' @param overwrite If TRUE, do not prompt for overwrite of \code{file}, with default as FALSE
#' @return The unmodified fduper object.
#' @examples
#' f %>% to_text_file("myfile.txt", overwrite=TRUE)
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

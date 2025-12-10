# Helper functions for biodata-utils


#' Custom `message` function
#'
#' @param ... Any number of objects to be pasted together.
#'
#' @returns A single character string output to the user in the console.

speak <- function(...) {
  message(paste0(...))
}


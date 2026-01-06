# Helper functions for biodata-utils


#' Custom `message` function
#'
#' @param ... Any number of objects to be pasted together.
#'
#' @returns A single character string output to the user in the console.

speak <- function(...) {
  message(paste0(...))
}


#' Custom `warning` function
#'
#' @param ... Any number of objects to be pasted together in a warning message.
#'
#' @returns A single character string output to the user in the console.

speak_warning <- function(...) {
  warning(paste0(...), call. = F)
}


#' Custom `error` function
#'
#' @param ... Any number of objects to be pasted together in an error message.
#'
#' @returns A single character string output to the user in the console.

speak_stop <- function(...) {
  stop(paste0(...), call. = F)
}


check_type <- function(name, value, type_expected) {
  
  if(type_expected == "logical") {
    if(!is.logical(value)) {
      stop(sprintf("Invalid 'type' for %s, must be logical (T/F).", name))
    }
  }
  
  if(type_expected == "character") {
    if(!is.character(value)) {
      stop(sprintf("Invalid 'type' for %s, must be 'character.'", name))
    }
  }
  
}


#' Load column map
#'
#' @returns A data frame showing column mapping to be used for raw data checking.

load_col_map <- function() {
  path <- "data/sockeye/internal/col_map.csv"
  return(read.csv(path))
}


# prompt <- "Press <Enter> to continue or type %s to review results: "
# alt <- "R"
readline_enter <- function(prompt, alt = NULL) {
  
  check_type(deparse(substitute(prompt)), prompt, "character")
  
  prompt <- sprintf(prompt, sprintf("'%s'", alt))
  input <- readline(prompt) |> toupper()
  if(is.null(alt) & input != "") {
    speak("Invalid input!")
    readline_enter(prompt)
  }
  
  if(!is.null(alt) & !toupper(input) %in% c("", alt)) {
    speak("Invalid input!")
    readline_enter(prompt, alt)
  }
  
  if(input == alt) {
    return(T)
  }
  if(input == "") {
    return(F)
  }
}

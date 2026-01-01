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
#' @param ... Any number of objects to be pasted together.
#'
#' @returns A single character string output to the user in the console as a
#'          warning message.

speak_warn <- function(...) {
  warning(paste0(...))
}


#' Custom `stop` function
#'
#' @param ... Any number of objects to be pasted together.
#'
#' @returns A single character string output to the user in the console as an
#'          error message.

speak_stop <- function(...) {
  stop(paste0(...))
}


#' Check data-type
#' 
#' Checks the data-type of a variable depending on the user-specified expected 
#' data-type.
#'
#' @param name Name of the variable to be checked.
#' @param value Value of variable to be checked.
#'
#' @returns A message stating whether or not data-type passed checks.

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

readline_enter <- function(prompt) {
  
  check_type(deparse(substitute(prompt)), prompt, "character")
  
  input <- readline(prompt)
  if(input != "") {
    speak("Invalid input!")
    readline_enter(prompt)
  }
  return(T)
}

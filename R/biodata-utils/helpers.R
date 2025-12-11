# Helper functions for biodata-utils


#' Custom `message` function
#'
#' @param ... Any number of objects to be pasted together.
#'
#' @returns A single character string output to the user in the console.

speak <- function(...) {
  message(paste0(...))
}



#' Check data-type internals
#' 
#' Checks the data-type of a variable depending on the user-specified expected 
#' data-type. The function will provide a logical output depending on the result
#' of type-checking, which will be used to handle messaging by 
#' `check_type_output`.
#'
#' @param name Name of the variable to be checked.
#' @param value Value of variable to be checked.
#'
#' @returns A message stating data-type passed checks.

check_type_internal <- function(name, value, type_expected) {
  
  if(type_expected == "logical") {
    if(is.logical(value)) {
      message(sprintf("'%s' is logical.", name))
      return(T)
    } else {
      stop(sprintf("Invalid 'type' for %s, must be logical (T/F).", name))
      return(F)
    }
  }
  
  if(type_expected == "character") {
    if(is.character(value)) {
      message(sprintf("'%s' is character", name))
      return(T)
    } else {
      stop(sprintf("Invalid entry for %s, must be 'character.'", name))
      return(F)
    }
  }
  
}

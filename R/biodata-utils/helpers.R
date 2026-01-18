# Helper functions for biodata-utils

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

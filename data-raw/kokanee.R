# Title: Create `kokanee` data object ----

source("R/biodata-utils/helpers.R")


# Load kokanee data object ----
load_kokanee <- function(kokanee_path = "./data/kokanee/kokanee.Rdata") {
  
  if(file.exists(kokanee_path)) {
    message("Loading the kokanee data object...")
    load(kokanee_path)
    
  } else {
    
    input <- readline("The kokanee data object doesn't yet exist, press <Enter> to create it or <Esc> to exit:")
    
    # Validate input and execute create_kokanee
    if(input != "") {
      message("Invalid input!")
      load_kokanee()
    } else create_kokanee()
    
  }
}


# Create kokanee data object ----
create_kokanee <- function(path = "data/kokanee/kokanee.Rdata") {
  
  # Check if `kokanee` exists
  if(file.exists(path)) {
    input <- readline("The kokanee data object already exists at '", path, "', press <Enter> to overwrite or <Esc> to exit:")
    
    # Validate and handle input
    if(input != "") {
      message("Invalid input!")
      create_kokanee()
    }
  }
  
  # Load raw data into `kokanee`
  message("Loading raw data files...")
  sep <- load_sep_raw()
  sep_hist <- load_sep_hist_raw()
  kitimat <- load_kitimat_raw()
  nechako <- load_nechako_raw()
  rmis <- load_rmis_raw()
  
  kokanee <- list(sep = sep,
                  sep_hist = sep_hist,
                  kitimat = kitimat,
                  nechako = nechako,
                  rmis = rmis)
  
  # Save `kokanee` as single and separate objects
  message("> Saving kokanee.Rdata")
  saveRDS(kokanee, file = path)
  map2(kokanee, names(kokanee), \(x, y) {
    z <- paste0(y, ".Rdata")
    speak("> Saving ", z)
    dest <- gsub("kokanee.Rdata", z, path)
    saveRDS(x, dest)
  })
  
  message("Successfully created kokanee data object at '", path, "'")
  
}


## SEP ----
load_sep_raw <- function() {
  message("> SEP raw")
  load_dat("sep")
}

load_sep_raw_truncated <- function() {
  message("> SEP raw (truncated)")
  load_dat("sep", truncate = T)
}

load_sep_meta <- function() {
  message("> SEP meta")
  load_dat("sep", meta_only = T)
}


## SEP Historical ----
load_sep_hist_raw <- function() {
  message("> SEP historical")
  return(load_dat("sep_historical"))
}


## Kitimat R Hatchery ----
load_kitimat_raw <- function() {
  
  message("> Kitimat R Hatchery")
  return(load_dat("kitimat"))
}


## Neckako River - DFO ----
load_nechako_raw <- function() {
  message("> Neckako DFO/NFCP")
  return(load_dat("nechako"))
}


## RMIS ----
load_rmis_raw <- function() {
  message("> RMIS")
  
  # Check to see if object already exists
  result <- check_object("rmis")
  if(!isFALSE(result)) {
    
    return(result)
    
  } else {
    
    return(list(rls = load_dat("rls"),
                rcv = load_dat("rcv"))
    )
    
  }
}


# Load data files ----
load_dat <- function(dataset, meta_only = F, truncate = F) {
  
  # Argument checking
  if(!is.logical(meta_only)) stop("Invalid entry for 'meta_only', must be logical (T/F).")
  if(!is.logical(truncate)) stop("Invalid entry for 'truncate', must be logical (T/F).")
  
  # Check to see if object already exists
  result <- check_object(dataset)
  if(!isFALSE(result)) {
    
    return(result) 
    
  } else {
    
    # Get list of .csv and .xlsx files
    dat_dirs <- list.dirs("data")
    dataset <- paste0(dataset, "$")
    dat_dirs <- dat_dirs[grep(dataset, dat_dirs, ignore.case = T)]
    path <- list.files(dat_dirs)
    path <- path[c(grep("csv$", path), grep("xlsx$", path))]
    path <- file.path(dat_dirs, path)
    
    # Handle case where `metdata_only = T`
    if(meta_only == T) {
      warning("Loading metadata only! Specify `meta_only = F` to load raw data as well.")
      path <- path[grepl("metadata", path, ignore.case = T)]
    }
    
    return(purrr::map(path, choose_load))
  }
  
}


# Check if data object exists ----
check_object <- function(dataset) {
  obj_path <- file.path("data/kokanee", paste0(dataset, ".Rdata"))
  if(file.exists(obj_path)) {
    speak("Data object for ", dataset, " already exists! Loading now...\n")
    return(readRDS(obj_path))
  } else return(F)
}


# Choose load function ----
choose_load <- function(path, ...) {
  
  speak(path, "...")
  
  if(grepl("csv$", path)) {
    return(readr::read_csv(path, col_types = "c"))
  }
  if(grepl("xlsx$", path)) {
    return(readxl::read_xlsx(path, col_types = "text"))
  }
  
}


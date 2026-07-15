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
    input <- readline(paste0("The kokanee data object already exists at '", path, "'.\nPress <Enter> to overwrite or <Esc> to exit:"))
    
    # Validate and handle input
    if(input != "") {
      message("Invalid input!")
      create_kokanee()
    }
  }
  
  # Load raw data into `kokanee`
  message("Loading raw data files...")
  
  kokanee <- list(sep_enpro = load_dat("sep_enpro"),
                  sep_oto_therm = load_dat("sep_oto_therm"),
                  sep_cwt = load_dat("sep_cwt"),
                  sep_hist = load_dat("sep_hist"),
                  kitimat = load_dat("kitimat"),
                  nechako = load_dat("nechako"),
                  rmis = load_rmis())
  
  # Save `kokanee` as single and separate objects
  message("> Saving kokanee.Rdata")
  save(kokanee, file = path)
  map2(kokanee, names(kokanee), \(x, y) {
    z <- paste0(y, ".Rdata")
    message("> Saving ", z)
    dest <- gsub("kokanee.Rdata", z, path)
    save(x, file = dest)
  })
  
  message("Successfully created kokanee data object at '", path, "'")
  
}

# Load data files ----
load_dat <- function(dat_name, trunc_table = F) {
  
  output <- paste0("> ", dat_name)
  
  # Argument checking
  check_type(name = deparse(substitute(dat_name)), 
             value = dat_name, 
             type_expected = "character")
  check_type(name = deparse(substitute(trunc_table)), 
             value = trunc_table, 
             type_expected = "logical")
  
  # Check to see if object already exists
  result <- check_object_exists(dat_name)
  if(!isFALSE(result)) {
    return(result) 
  } else {
    
    # Get list of .csv and .xlsx files
    dat_dirs <- list.dirs("data")
    dat_name <- paste0(dat_name, "$")
    dat_dirs <- dat_dirs[grep(dat_name, dat_dirs, ignore.case = T)]
    dat_name <- gsub("\\$", "", dat_name)
    dat_files <- list.files(dat_dirs)
    dat_files <- dat_files[c(grep("csv$", dat_files), grep("xlsx$", dat_files))]
    path <- file.path(dat_dirs, dat_files)
    
    # Handle case where `trunc_table = T`
    if(trunc_table == T) {
      dat <- purrr::map(path, \(x) {
        df <- choose_load(x, n_max = 5)
        df$dat_name <- dat_name
        return(df)
      })
      return(dat)
    }
    
    dat <- purrr::map(path, \(x) {
      df <- choose_load(x)
      df$dat_name <- dat_name
      return(df)
    })
    
    names(dat) <- dat_files
    
    return(dat)
  }
  
}

## Load RMIS data files ----
load_rmis <- function(...) {
  message("> RMIS")
  
  # Check to see if object already exists
  result <- check_object_exists("rmis")
  if(!isFALSE(result)) {
    
    return(result)
    
  } else {
    
    return(list(rls = load_dat("rls", ...),
                rcv = load_dat("rcv", ...))
    )
    
  }
}


# Check if data object exists ----
check_object_exists <- function(dat_name) {
  obj_path <- file.path("data/kokanee", paste0(dat_name, ".Rdata"))
  if(file.exists(obj_path)) {
    message("Data object for ", dat_name, " already exists! Loading now...\n")
    return(load(obj_path))
  } else return(F)
}


# Choose load function ----
choose_load <- function(path, n_max = Inf, ...) {
  
  message("Loading... ", path)
  
  if(grepl("csv$", path)) {
    dat <- readr::read_csv(path, col_types = "c", n_max = n_max)
  }
  if(grepl("xlsx$", path)) {
    dat <- readxl::read_xlsx(path, col_types = "text", n_max = n_max)
  }
  
  return(dat |> mutate(path = path))
  
}


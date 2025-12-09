# Title: Create `kokanee` data object ----

# Load data ----

# Load kokanee data object ----
load_kokanee <- function() {
  message("Loading the kokanee data object...")
  load("./data/kokanee/kokanee.Rdata")
}

# Create kokanee data object ----
create_kokanee <- function(path = "data/kokanee/kokanee.Rdata") {
  
  if(file.exists(path)) {
    speak <- paste0("The kokanee data object already exists at '", path, "'")
    stop(speak)
  }
  message("Loading raw data files...")
  sep_raw <- load_sep_raw()
  sep_hist_raw <- load_sep_hist_raw()
  kitimat_raw <- load_kitimat_raw()
  nechako_raw <- load_nechako_raw()
  rmis_raw <- load_rmis_raw()
  save.image(file = path)
  message("Successfully created kokanee data object at '", path, "'")
  
}


## SEP ----
load_sep_raw <- function() {
  message("> SEP raw")
  load_dat("sep")
}

load_sep_meta <- function() {
  message("> SEP meta")
  load_dat("sep", meta_only = T)
}

# load_sep_raw <- function() {
#   dat_dirs <- list.dirs("data")
#   dat_dirs <- dat_dirs[grepl("sep$", dat_dirs, ignore.case = T) == T]
#   path <- list.files(dat_dirs)
#   path <- file.path(dat_dirs, path)
#   return(purrr::map(path, choose_load, col_types = "text"))
# }


## SEP Historical ----
load_sep_hist_raw <- function() {
  message("> SEP historical")
  return(load_dat("sep_historical"))
}

# load_sep_hist_raw <- function() {
#   dat_dirs <- list.dirs("data")
#   dat_dirs <- dat_dirs[grepl("sep_historical$", dat_dirs, ignore.case = T) == T]
#   path <- list.files(dat_dirs)
#   path <- file.path(dat_dirs, path)
#   return(purrr::map(path, choose_load, col_types = "c"))
# }

## Kitimat R Hatchery ----
load_kitimat_raw <- function() {
  
  message("> Kitimat R Hatchery")
  return(load_dat("kitimat"))
}

# load_kitimat_raw <- function() {
#   dat_dirs <- list.dirs("data")
#   dat_dirs <- dat_dirs[grepl("kitimat", dat_dirs, ignore.case = T) == T]
#   path <- list.files(dat_dirs)
#   path <- file.path(dat_dirs, path)
#   return(purrr::map(path, choose_load, col_types = "c"))
# }


## Neckako River - DFO ----
load_nechako_raw <- function() {
  message("> Neckako DFO")
  return(load_dat("nechako"))
}

# load_nechako_raw <- function() {
#   dat_dirs <- list.dirs("data")
#   dat_dirs <- dat_dirs[grepl("nechako", dat_dirs, ignore.case = T) == T]
#   path <- list.files(dat_dirs)
#   path <- file.path(dat_dirs, path)
#   return(purrr::map(path, choose_load, col_types = "c"))
# }


## RMIS ----
load_rmis_raw <- function() {
  message("> RMIS")
  return(list(rls = load_dat("rls"),
              rcv = load_dat("rcv"))
         )
}

# dat_dirs <- list.dirs("data")
# ## Load releases
# rls <- read.csv("data/kokanee/RMIS/rls_50_21.csv")
# 
# ## Load recovery files
# my_dir <- "data/kokanee/RMIS/rcv/"
# list.files(my_dir)
# rcv <- paste0(my_dir, list.files(my_dir)) %>%
#   map_df(~read_csv(., col_types = paste(c(rep("c", 43)), collapse = "")))


# Load data files ----
load_dat <- function(dataset, meta_only = F) {
  dat_dirs <- list.dirs("data")
  dataset <- paste0(dataset, "$")
  dat_dirs <- dat_dirs[grepl(dataset, dat_dirs, ignore.case = T) == T]
  path <- list.files(dat_dirs)
  path <- file.path(dat_dirs, path)
  
  if(meta_only == T) {
    warning("Loading metadata only! Specify `meta_only = F` to load raw data as well.")
    path <- path[grepl("metadata", path, ignore.case = T)]
  }
  
  return(purrr::map(path, choose_load))
}

# Choose load function ----
choose_load <- function(path, ...) {
  
  speak <- paste0(path, "...")
  message(speak)
  
  if(grepl("csv$", path)) {
    return(readr::read_csv(path, col_types = "c"))
  }
  if(grepl("xlsx$", path)) {
    return(readxl::read_xlsx(path, col_types = "text"))
  }
  warning(paste("Not a path to a valid data file:", path))
}


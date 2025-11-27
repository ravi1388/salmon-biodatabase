# Title: Create `kokanee` data object ----

# Setup environment ----
rm(list=ls())

# Load data ----

# Create kokanee data object ----
create_kokanee <- function() {
  sep_raw <- load_sep_raw()
  sep_hist_raw <- load_sep_hist_raw()
  kitimat_raw <- load_kitimat_raw()
  nechako_raw <- load_nechako_raw()
  rmis_raw <- load_rmis_raw()
  path <- "data/kokanee/kokanee.Rdata"
  save.image(file = path)
}


## SEP ----
load_sep_raw <- function() {
  load_dat_raw("sep")
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
  return(load_dat_raw("sep_historical"))
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
  return(load_dat_raw("kitimat"))
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
  return(load_dat_raw("nechako"))
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
  return(list(rls = load_dat_raw("rls"),
              rcv = load_dat_raw("rcv"))
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


# Load file ----
load_dat_raw <- function(dataset) {
  dat_dirs <- list.dirs("data")
  dataset <- paste0(dataset, "$")
  dat_dirs <- dat_dirs[grepl(dataset, dat_dirs, ignore.case = T) == T]
  path <- list.files(dat_dirs)
  path <- file.path(dat_dirs, path)
  return(purrr::map(path, choose_load))
}

# Choose load function ----
choose_load <- function(path, ...) {
  
  speak <- paste0("Now loading... ", path)
  print(speak)
  
  if(grepl("csv$", path)) {
    return(readr::read_csv(path, col_types = "c"))
  }
  if(grepl("xlsx$", path)) {
    return(readxl::read_xlsx(path, col_types = "text"))
  }
  warning(paste("Not a path to a valid data file:", path))
}

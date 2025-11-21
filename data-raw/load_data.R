# Title: Create `kokanee` data object ----

# Setup environment ----
rm(list=ls())

# Load data ----

dat_dirs <- list.dirs("data")

## EnPro ----
path <- list.files(dat_dirs[7])
path <- file.path(dat_dirs[7], path[c(3, 6)])
enpro_dat <- purrr::map(path, readxl::read_xlsx, col_types = "text")


## Kitimat R Hatchery ----
path <- list.files(dat_dirs[3])
path <- file.path(dat_dirs[3], path)
kit_dat <- purrr::map(path, read_csv, col_types = "c")


## Neckako River - DFO ----
ndfo_dat <- read.csv("data/kokanee/NECHAKO/nechako_dfo.csv")


## Robertson Cr Historical ----
rh_dat <- read.csv("data/kokanee/SEP/SEP_HISTORICAL/SEP_HISTORICAL_CN_ROBERTSON.csv")


## Chilliwack R Historical ----
ch_dat <- read.csv("data/kokanee/SEP/SEP_HISTORICAL/SEP_HISTORICAL_CN_CHILLIWACK.csv")


## RMIS ----
## Load releases
rls <- read.csv("data/kokanee/RMIS/rls_50_21.csv")

## Load recovery files
my_dir <- "data/kokanee/RMIS/rcv/"
list.files(my_dir)
rcv <- paste0(my_dir, list.files(my_dir)) %>%
  map_df(~read_csv(., col_types = paste(c(rep("c", 43)), collapse = "")))


# Save data object ----
path <- "data/kokanee/kokanee.Rdata"
save.image(file = path)

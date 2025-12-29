# Title: Tidy Kitimat
# Author: Ravi Maharaj
# Date: 2025-11-07

# Description ----
#' This script contains code used to:
#' 1) Run QAQC functions to determine what standardization is required for raw
#'    data.
#' 2) Run the required functions to standardize like variables in raw data.
#' 3) Add standardized data to `sockeye` biodatabase.

# Packages ----
library(tidyr)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)
library(janitor)
library(lubridate)

source("R/kokanee/kokanee.R")
source("R/biodata-utils/helpers.R")
source("R/biodata-utils/qaqc_funs2.R")

# Load data ----
# load_kokanee()

kit_work <- load_kitimat_raw() |> 
  map(clean_names)

# Data QAQC ----
#' Run functions to standardize dataset(s) before they are compiled and/or added
#' to the `sockeye` biodatabase:
#' - Column names among like columns
#' - Data types among like columns
#' - Values among like columns

dataset <- kit_work
dat_name <- "kitimat"
attr_type <- "names"

col_attr <- get_col_attr(dataset, dat_name, attr_type)

col_match <- match_col_attr(col_attr, dat_name, load_col_map(), attr_type)

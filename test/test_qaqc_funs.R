# Title: Tidy Kitimat
# Author: Ravi Maharaj
# Date: 2025-11-07

# Description ----
#' This script contains code used to:
#' 1) Run QAQC functions to determine what standardization is required for raw
#'    data.
#' 2) Run the required functions to standardize like variables in raw data.
#' 3) Add standardized data to `sockeye` biodatabase.

# Setup environment ----
rm(list = ls())

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
source("R/biodata-utils/qaqc_funs.R")

# Load data ----
# load_kokanee()

kit_work <- load_kitimat_raw() |> 
  map(clean_names)

# Load data ----
#' Run functions to standardize dataset(s) before they are compiled and/or added
#' to the `sockeye` biodatabase:
#' - Column names among like columns
#' - Data types among like columns
#' - Values among like columns

dataset <- kit_work
col_map <- load_col_map()

# Make QAQC object ----
qaqc_object <- make_qaqc_object(dataset)
qaqc_status(qaqc_object)


# Run QAQC functions ----
## 'names'
qaqc_object_attr <- get_col_attr(qaqc_object)
qaqc_status(qaqc_object_attr)

qaqc_object_match <- match_col_attr(qaqc_object_attr)
qaqc_status(qaqc_object_match)

qaqc_object_result <- get_qaqc_result(qaqc_object_match)
qaqc_status(qaqc_object_result)
qaqc_result_summary(qaqc_object_result)

qaqc_object <- advance_qa_stage(qaqc_object_result)
qaqc_status(qaqc_object)

## 'types'
qaqc_object_attr <- get_col_attr(qaqc_object)
qaqc_status(qaqc_object_attr)

qaqc_object_match <- match_col_attr(qaqc_object_attr)
qaqc_status(qaqc_object_match)

qaqc_object_result <- get_qaqc_result(qaqc_object_match)
qaqc_status(qaqc_object_result)
qaqc_result_summary(qaqc_object_result)

qaqc_object <- advance_qa_stage(qaqc_object_result)
qaqc_status(qaqc_object)


# Debug `apply_newattr` ----
dat_name <- qaqc_object_match$dat_name
target_attr <- qaqc_object_match$target_attr
dataset <- qaqc_object_match$dataset
col_match <- qaqc_object_match$col_match

col_match_sub <- col_match[[1]]
dataset_sub <- dataset[[1]]

this_col <- dataset_sub[[4]]
this_col_name <- names(dataset_sub)[4]
new_attr <- col_attr_new[4]
source_attr <- col_attr_source[4]


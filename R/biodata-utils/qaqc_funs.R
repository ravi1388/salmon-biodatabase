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

source("data-raw/kokanee.R")
source("R/biodata-utils/helpers.R")

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

## Column names ----

get_col_names <- function(dataset, dat_name) {
  
  df <- modify(dataset, \(x) {
    x <- tibble(names(x),
                unique(x$path))
    names(x) <- c(paste0("colnames_", dat_name), "path")
    return(x)
  })
  
  return(df)
}


match_col_names <- function(col_names, col_map) {
  col_match <- map(col_names,\(x) {
    df <- left_join(x, col_map)
    these_cols <- names(df)
    new_cols <- map(these_cols, \(x) {
      if(!x %in% c("path", "colnames_sock", "class_sock")) {
        return("colnames_dat")
      } else return(x)
    })
    names(df) <- new_cols
    return(df)
  })
  
  col_match <- modify(col_match, \(x) {
    x <- x |> 
      mutate(result = case_when(colnames_dat == colnames_sock ~ "matched...",
                                colnames_dat != colnames_sock ~ "mapped...",
                                is.na(colnames_sock) ~ "new..."),
             result = factor(result, levels = c("matched...", "mapped...", "new...")))
  })
}

get_match_result <- function(col_match, dataset, dat_name) {
  
  match_result <- col_match |> bind_rows() |> arrange(result) |> 
    select(-colnames_sock)
  
  match_result_sum <- modify(col_match, \(x) {
    table(x$result)
  }) |> bind_rows() |> colSums()
  
  dataset_newnames <- map2(col_match, dataset, \(x, y) {
    x <- x |> 
      mutate(colnames_new = ifelse(result == "new...", 
                                   colnames_dat, colnames_sock)) |> 
      pull(colnames_new)
    names(y) <- x
    return(y)
  })
  
  speak("Name check complete for '", str_to_title(dat_name), "'.")
  speak(names(match_result_sum), " ", match_result_sum, "\n")
  
  result <- readline_enter("Press <Enter> to continue or type 'R' to review results:")
  result <- toupper(result)
  if(result == "R") {
    print(match_result)
    return(dataset_newnames)
  } else if(isTRUE(result)) {
    return(dataset_newnames)
  }
}

standardize_col_names <- function(dataset, dat_name, col_map = load_col_map()) {
  col_names <- get_col_names(dataset, dat_name)
  
  col_match <- match_col_names(col_names, col_map)
  
  match_result <- get_match_result(col_match, dataset, dat_name)
  
  return(match_result)
  
}


## Column data types ----
 get_col_class <- function(dataset) {
   
   col_class <- dataset |> 
     map(\(x) {
       ls <- as.list(x[1,])
       col_class <- map(ls, \(y) {
         y <- class(y)
       }) |> 
         bind_cols() |> 
         pivot_longer(names(x),
                      names_to = "colnames_sock",
                      values_to = "class_dat")
       return(col_class)
     }) |> 
     bind_rows()
   
   return(col_class)
 }


## Column values

### Get unique values of columns
#' Similar columns:
#' - 'sex_final' & 'sex_m_f_j'
#' 
#' Age columns represent different age values
#' - 'cwt_age' taken from year of release
#' - 'scale_age' taken from scale readings as G-R age
col_vals <- map2(kit_work, old_cols, \(df, cols) {
  map(cols, \(cols){
    unique(df[cols])
  })
})



rm(old_cols, new_cols)

## Impute 'pohl' ----
## Check composition of entries
kit_work |> 
  map(\(x) {
    x |> 
      pull(post_orbital_hypural_poh) |> 
      table()
  })

## Check composition of non-numeric values
#' kit_work[[2]] contains 382 entries as '-'
kit_work |> 
  map(\(x) {
    x |> 
      filter(!grepl("[0-9]", post_orbital_hypural_poh)) |>
      pull(post_orbital_hypural_poh) |> 
      table()
  })

## Fix erroneous values
### Get non-numeric values
these_values <- kit_work |> 
  modify(\(x) {
    x |> 
      filter(!grepl("[0-9]", post_orbital_hypural_poh)) |>
      mutate(post_orbital_hypural_poh = as.character(post_orbital_hypural_poh)) |> 
      pull(post_orbital_hypural_poh) |> unique()
  }) |> 
  unlist()

## Replace non-numeric values with `NA`s
kit_work <- kit_work |> 
  map(\(x) {
    x |> 
      mutate(post_orbital_hypural_poh = ifelse(post_orbital_hypural_poh %in% these_values, 
                                               NA, as.character(post_orbital_hypural_poh)))
  })

## Confirm resolved errors from non-numeric values
kit_work |> 
  map(\(x) {
    x |> 
      filter(!grepl("[0-9]", post_orbital_hypural_poh)) |>
      pull(post_orbital_hypural_poh) |> 
      table()
  })


## Combine datasets ---
kit_work <- kit_work |> bind_rows()


## Impute 'age_ocean' ----
### Tidy cwt_meta from sep_meta ---
#' Code in this chunk used to clean cwt_meta will be moved to '01_tidy_sep.R'
cwt_work <- load_sep_cwt() #|> 
  map(clean_names)

cwt_meta <- cwt_meta[[1]]

### Impute tag_code
#### Get columns containing cwt tag number
#' - tagcode_1
#' - tagcode_3
cwt_meta_cols <- cwt_meta |> names()
cwt_meta_cols[grepl("tagcode", cwt_meta_cols) == T]

#### Compare 'tagcode_1' and 'tagcode_3'
#' These columns are essentially the same, use either
cwt_meta |> 
  distinct(tagcode_3, tagcode_1) |> 
  mutate(across(c("tagcode_1", "tagcode_3"), \(x) {
    x |> as.character() |>
      str_trim() |>
      str_replace_all("\\*", "")
  }),
  match = ifelse(str_detect(tagcode_3, tagcode_1),
                        T, F)) |> 
  filter(match == F)

cwt_meta <- cwt_meta |> distinct(release_year, brood_year, tagcode_1, tagcode_3)

non_num_cwt <- cwt_meta |> 
  filter(is.na(as.numeric(tagcode_1))) |> 
  pull(tagcode_1) |> unique()

kit_work |> 
  filter(is.na(as.numeric(tag_code))) |>
  pull(tag_code) |> unique()

### Add cwt_meta to kit_work
kit_work <-
  left_join(kit_work, cwt_meta, by = c("tag_code" = "tagcode_3", "year")) |> 
    filter(!is.na(brood_year))

### Impute 'age_gr'
kit_work$scale_age |> unique()

kit_work |> 
  mutate(fw_age = release_year - brood_year,
         age_gr_impute = ifelse(!is.na(brood_year) & scale_age == "-",
                                ))


kit_work$Age <- as.character(kit_work$Age)

kit_work2 <- kit_work2[,-which(is.na(names(kit_work2)))]

kit_work <- rbind.data.frame(kit_work, kit_work2)

# Postorbital-hypural length ####
sort(unique(kit_work$POHL))
kit_work$POHL_clean <- as.numeric(kit_work$POHL)

# Sex ####
kit_work |> 
  map(\(x) unique(x$sex))

kit_work$enpro_sex_final_impute[kit_work$SEX...FINAL=="M"] <- "MALE"
kit_work$enpro_sex_final_impute[kit_work$SEX...FINAL=="F"] <- "FEMALE"
kit_work$enpro_sex_final_impute[kit_work$SEX...FINAL=="J"] <- "MALE"

# Ocean Age ####
kit_work$ocean_age[kit_work$Age=="2"] <- "1-ocean"
kit_work$ocean_age[kit_work$Age=="21"] <- "1-ocean"
kit_work$ocean_age[kit_work$Age=="32"] <- "1-ocean"
kit_work$ocean_age[kit_work$Age=="3"] <- "2-ocean"
kit_work$ocean_age[kit_work$Age=="31"] <- "2-ocean"
kit_work$ocean_age[kit_work$Age=="42"] <- "2-ocean"
kit_work$ocean_age[kit_work$Age=="4"] <- "3-ocean"
kit_work$ocean_age[kit_work$Age=="41"] <- "3-ocean"
kit_work$ocean_age[kit_work$Age=="52"] <- "3-ocean"
kit_work$ocean_age[kit_work$Age=="5"] <- "4-ocean"
kit_work$ocean_age[kit_work$Age=="51"] <- "4-ocean"
kit_work$ocean_age[kit_work$Age=="62"] <- "4-ocean"
kit_work$ocean_age[kit_work$Age=="6"] <- "5-ocean"
kit_work$ocean_age[kit_work$Age=="7"] <- "6-ocean"


kit_work$ocean_age[kit_work$enpro_sex_final_impute=="Male" &
                    kit_work$POHL_clean<500] <- "1-ocean"
kit_work$ocean_age[kit_work$enpro_sex_final_impute=="Male" &
                    kit_work$ocean_age=="21" &
                    kit_work$POHL_clean>500] <- "2-ocean"

# Life stage ####
kit_work$Life_Stage_clean[kit_work$SEX...FINAL == "J"] <- "Jack"
kit_work$Life_Stage_clean[kit_work$ocean_age=="1-ocean" & kit_work$enpro_sex_final_impute=="Male"] <- "Jack"
kit_work$Life_Stage_clean[kit_work$SEX...FINAL == "F"] <- "Adult"
kit_work$Life_Stage_clean[kit_work$SEX...FINAL == "M"] <- "Adult"

# Stock ####
kit_work$enpro_site_river_location_impute <- "Kitimat R"
kit_work$stock_impute <- kit_work$enpro_site_river_location_impute

# Database ####
kit_work$Database <- "Kitimat R Hatchery"

intersect(names(cnbiodata_clean), names(kit_work))

df <- kit_work[,names(cnbiodata_clean)]


dest <- "data/sockeye/KIT_BIODATA.csv"
write.csv(kit_work, dest)
dest <- "data/sockeye/KIT_BIODATA.Rdata"
save(kit_work, file = dest)

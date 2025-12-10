# Title: 02 - Tidy Kitimat
# Author: Ravi Maharaj
# Date: 2025-11-07

# Description ----
#' This script contains code used to:
#' 1) Clean and standardize variables in raw data for KITIMAT_CN_1 and
#'    KITIMAT_CN_2.
#' 2) Create a new table KITIMAT_BIODATA, to be stored in the `sockeye` data 
#'    object.

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

# Load data ----
# load_kokanee()

kit_work <- load_kitimat_raw() |> 
  map(clean_names)

# Data cleaning ----
## Standardize column names

### Get unique values of columns
#' Similar columns:
#' - 'sex_final' & 'sex_m_f_j'
#' 
#' Age columns represent different age values
#' - 'cwt_age' taken from year of release
#' - 'scale_age' taken from scale readings as G-R age
kit_work |> 
  modify(names) |> 
  modify(sort)

old_cols <- kit_work |> 
  modify(names)

col_vals <- map2(kit_work, old_cols, \(df, cols) {
  map(cols, \(cols){
    unique(df[cols])
  })
})

### Standardize names for similar variables between dataframes
new_cols <- map(old_cols, \(cols) {
  map_vec(cols, \(cols) {
    if(grepl("sex", cols)) {
      cols <- "sex"
    } else cols <- cols
  })
}) 

### Set new column names
names(kit_work[[1]]) <- new_cols[[1]]
names(kit_work[[2]]) <- new_cols[[2]]

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
cwt_meta <- load_sep_meta() |> 
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

# Title: 01 - Tidy SEP
# Author: Ravi Maharaj
# Date: 2025-11-07

# Description ----
#' This script contains code used to:
#' 1) Clean and standardize variables stored in the raw data tables SEP_ENPRO_CN 
#'    and SEP_OTO_THERM_CN.
#' 2) Create a new table SEP_BIODATA, to be stored in the `sockeye` data object.

# Packages ----
library(tidyr)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)
library(janitor)
library(lubridate)

# Load data ----

# load("./data/kokanee/kokanee.Rdata")

# enpro_dat <- readxl::read_xlsx("data/kokanee/SEP/SEP_ENPRO_CN.xlsx")
# enpro_meta <- readxl::read_xlsx("data/kokanee/SEP/SEP_ENPRO_CN_METADATA.xlsx")
# oto_dat <- readxl::read_xlsx("data/kokanee/SEP/SEP_OTO_THERM_CN.xlsx")

# Data cleaning ----

#------------------#
## SEP - ENPRO ----
#------------------#


### Check for dupes ----
enpro_work <- clean_names(enpro_dat)

names(enpro_work) |> sort()

dupes_enpro <- enpro_work |>
  # select(-oto_stock, -hatch_code) |> 
  distinct() |> 
  get_dupes(id,
            catch_date_end, catch_date_start, 
            cwt_head_label, cwt_tag_code,
            enpro_locatioin,
            sample_number, sample_num, 
            sample_start_date, sample_start_dt, sample_end_date, sample_end_dt,
            scale_book_number, scale_book_specimen_number, 
            tag_number)

#### Variables responsible for dupes:
#' - hatch_code
#' - oto_stock
#' 
#' Duplicate records contain value/NA pairs that need to be consolidated

enpro_dupe_ids <- dupes_enpro$id |> unique()

keep_rows <- enpro_work |> 
  filter(id %in% enpro_dupe_ids & (!is.na(oto_stock) | !is.na(hatch_code)))

enpro_work <- enpro_work |> 
  filter(!id %in% enpro_dupe_ids) |> 
  bind_rows(keep_rows)


### Impute adipose_fin_clip ----
enpro_work$adipose_fin_clip |> tolower() |> unique() |> sort()

enpro_work <- enpro_work |> 
  mutate(adipose_fin_clip_impute = adipose_fin_clip |> tolower(),
         adipose_fin_clip_impute = case_when(
           adipose_fin_clip_impute %in% c("ad", "yes", "y", "rv", "adrv") ~ "yes",
           adipose_fin_clip_impute %in% c("unch", "adlv", "no", "n", "nomk") ~ "no"))

unique(enpro_work$adipose_fin_clip_impute)


### Impute sex_final ----
enpro_work$sex_final |> tolower() |>  unique() |> sort()

enpro_work <- enpro_work |> 
  mutate(sex_final_impute = sex_final |> tolower(),
         sex_final_impute = case_when(
           sex_final_impute %in% c("unspecified", "unspecified", "undetermined", "u/k") ~ "unknown",
           TRUE ~ sex_final_impute))

enpro_work$sex_final_impute |> tolower() |>  unique() |> sort()


### Impute site_river_location ----
sort(unique(enpro_work$site_river_location))

#### Add abbreviations for River, Creek, Cove, Lower and Upper
enpro_work <- enpro_work |> 
  mutate(site_river_location_impute = str_to_title(site_river_location),
         site_river_location_impute = str_replace(site_river_location_impute, "River", "R"),
         site_river_location_impute = str_replace(site_river_location_impute, "Creek", "Cr"),
         site_river_location_impute = str_replace(site_river_location_impute, "Cove", "Cv"),
         site_river_location_impute = str_replace(site_river_location_impute, "Lower", "Low"),
         site_river_location_impute = str_replace(site_river_location_impute, "Upper", "Up"))

sort(unique(enpro_work$site_river_location_impute))

#### Consolidate names:
#' "Atnarko R Low", "Atnarko R Mid" & "Atnarko R Up" into "Atnarko R"
#' "Tsu-Ma_Uss" & "Somass R" to "Somass R"
enpro_work <- enpro_work |> 
  mutate(site_river_location_impute = case_when(
    site_river_location_impute %in% c("Atnarko R Low", "Atnarko R Mid", "Atnarko R Up") ~ "Atnarko R",
    site_river_location_impute == "Tsu-Ma_uss" ~ "Somass R",
    TRUE ~ site_river_location_impute)) |> 
  mutate(site_river_location_impute = case_when(grepl("Qual", site_river_location_impute) ~ "B+L Qualicum R",
                                                grepl("Nanaimo R Low Fall", site_river_location_impute) ~ "Nanaimo R",
                                                grepl("Nanaimo R Up Summer", site_river_location_impute) ~ "Nanaimo R",
                                                grepl("Great Central Lake Fishway", site_river_location_impute) ~ "Robertson Cr",
                                                TRUE ~ site_river_location_impute))

sort(unique(enpro_work$site_river_location_impute))
rm(dupes_enpro, keep_rows, enpro_dupe_ids)


#--------------------------------------#
## SEP - Otolith/Thermal Mark Data ----
#--------------------------------------#

oto_work <- oto_dat |> clean_names()

### Check for dupes ----
dupes_oto <- oto_work |> 
  select(id, stock_of_origin) |> 
  filter(!is.na(id)) |> 
  janitor::get_dupes()

### Impute stock_of_origin ----
sort(unique(oto_work$stock_of_origin))

#### Consolidate values in stock_of_origin
#' Convert all "" values to NA
#' Combine "Atnarko R Low", "Atnarko R Up" & "Atnarko R" into "Atnarko R"
#' Combine "First Lk/GSVI" & "Nanaimo R" into "Namaimo R"
oto_work <- oto_work |> 
  mutate(stock_of_origin_impute = str_trim(stock_of_origin),
         stock_of_origin_impute = case_when(
           stock_of_origin_impute == "First Lk/GSVI" ~ "Nanaimo R",
           stock_of_origin_impute %in% c("Atnarko R Low", "Atnarko R Up") ~ "Atnarko R",
           stock_of_origin_impute == "" ~ NA,
           TRUE ~ stock_of_origin_impute
         )) |> 
  mutate(stock_of_origin_impute = case_when(grepl("Qual", stock_of_origin_impute) ~ "B+L Qualicum R",
                                            grepl("Capilano\\+Chill", stock_of_origin_impute) ~ "Capilano R",
                                            grepl("Chill\\+Harrison R", stock_of_origin_impute) ~ "Chilliwack R",
                                            grepl("Salmon R/JNST", stock_of_origin_impute) ~ "Salmon R",
                                            TRUE ~ stock_of_origin_impute))

oto_work |> distinct(stock_of_origin, stock_of_origin_impute) |> 
  mutate(diff = ifelse(stock_of_origin_impute != stock_of_origin, T, F)) |> 
  filter(diff == T)
# print(n=100)


#### Reduce OTO to its unique variables and foreign key 'id'
names_enpro <- names(enpro_work)
names_oto <- names(oto_work)
match_names <- intersect(names_enpro, names_oto)
setdiff(names_enpro, match_names)
#' OTO contains all but 7 of the columns from ENPRO

#' Tag all untagged ENPRO variables
names_enpro <- purrr::modify(names_enpro, \(x) {
  if(!grepl('enpro', x) & x != 'id') {
    paste0('enpro_', x)
  } else {
    x
  }
})

#' Tag variables unique to OTO
names_oto <- purrr::modify(names_oto, \(x) {
  if(!x %in% match_names) {
    paste0('oto_', x)
  } else {
    x
  }
})

#' Check for correct order of variable names, order correct if output is ""
modify2(names_enpro, names(enpro_work), \(x, y) {
  z <- gsub(y, "", x)
  gsub("enpro_", "", z)
}) |> unique()
modify2(names_oto, names(oto_work), \(x, y) {
  z <- gsub(y, "", x)
  gsub("oto_", "", z)
}) |> unique()

#' Set names
names(enpro_work) <- names_enpro
names(oto_work) <- names_oto

#' Select unique columns for OTO with foreign key
names_oto <- c("id", names_oto[grepl("oto_", names_oto)])
oto_work <- oto_work |> 
  select(all_of(names_oto))
names(oto_work)


#------------------#
## SEP Biodata ----
#------------------#

### Create SEP Biodata table ----
#' Join OTO and ENPRO tables to get common observations
sep_biodata <- oto_work |>
  right_join(enpro_work, 
             by = "id")

### Clear unneeded variables
rm(dupes_oto, match_names, names_enpro, names_oto)

### Impute stock ----
#' Use `stock_of_origin_impute` with NA values replaced with those from 
#' `site_river_location_impute`.
#' 
#' After standardizing both for uniformity below: 
#' - *~60%* of observations have `NA` values for `stock_of_origin_impute`
#' - The correspondence rate between `stock_of_origin_impute` and 
#'   `site_river_location_impute` is *~94.8%*

#### Check percent of observations with NA stock_of_origin_impute
sep_biodata |> 
  group_by(oto_stock_of_origin_impute) |> 
  summarise(n_obs_stk = n()) |> 
  ungroup() |> 
  mutate(n_obs_all = sum(n_obs_stk),
         p_obs_stk = 100*(n_obs_stk/n_obs_all)) |> 
  arrange(desc(p_obs_stk)) |> 
  print(n = 100)
#' ~60% of observations have NA values for stock_of_origin


#### Check correspondence between oto_stock_of_origin_impute and enpro_site_river_location_impute
check_correspondance <- sep_biodata |> 
  select(oto_stock_of_origin_impute, enpro_site_river_location_impute) |> 
  filter(!is.na(oto_stock_of_origin_impute)) |> 
  mutate(diff = ifelse(oto_stock_of_origin_impute == enpro_site_river_location_impute,
                       "same", "diff")) #|> 
# distinct() |> filter(diff == T)

check_correspondance |>
  group_by(diff) |> 
  mutate(n_pair = n()) |> ungroup() |>
  distinct(diff, n_pair) |> 
  pivot_wider(values_from = n_pair, names_from = diff) |> 
  mutate(p_diff = 100*(diff/sum(same, diff)))

#' `oto_stock_of_origin_impute` and `enpro_site_river_location_impute` differ in ~5.19% of 
#' observations with values for both variables.
#' 
#' Assuming that all fish will return to their home waterbody, location where 
#' fish were sampled, `enpro_site_river_location_impute`, used to fill `NA` 
#' values in `oto_stock_of_origin_impute` to create `stock_impute`.
#' 
#' A new variable, `stock_impute_flag`, will be used to indicate which values 
#' for `stock_impute` are real and imputed using the values 'actual' and
#' 'imputed' respectively.

sep_biodata_work <- sep_biodata |>
  mutate(stock_impute_flag = ifelse(is.na(oto_stock_of_origin_impute),
                                    "imputed",
                                    "actual"),
         stock_impute = ifelse(is.na(oto_stock_of_origin_impute),
                               enpro_site_river_location_impute, 
                               oto_stock_of_origin_impute))

sep_biodata_work |> 
  filter(is.na(stock_impute)) |> nrow()
sep_biodata_work$stock_impute_flag |> table()

rm(check_correspondance)


### Impute final_use_distribution ----
unique(sep_biodata_work$enpro_final_use_distribution)

sep_biodata_work <- sep_biodata_work |> 
  mutate(final_use_distribution_impute = case_when(
    enpro_final_use_distribution == "Natural Spawners" ~ "Natural Spawner",
    enpro_final_use_distribution == "Not Applicable" ~ NA,
    enpro_final_use_distribution == "Unknown" ~ NA,
    TRUE ~ enpro_final_use_distribution
  ))

unique(sep_biodata_work$final_use_distribution_impute)


### Impute post_orbital_hypural_poh ----
### Check data type
typeof(sep_biodata_work$enpro_post_orbital_hypural_poh)
### Check for observations with NA values
sep_biodata_work$enpro_post_orbital_hypural_poh |> table(useNA = "ifany")


### Impute 'resolved_age' ----
#' 'resolved_age' contains data from 'age_gr' that have been double-checked for
#' errors by cross-referencing against scale book data. As a result it contains 
#' `NA`s corresponding to unchecked 'age_gr' values and known unresolved errors,
#' specifically for Cowichan R data.
#' 
#' First, known errors in Cowichan R data were corrected, then the remaining
#' missing values were filled in using 'age_gr' (Gilbert-Rich age) and estimated 
#' Gilbert-Rich age.

#### Fix errors in 'enpro_resolved_age' ----
sep_biodata_work$enpro_resolved_age |> unique()
sep_biodata_work <- sep_biodata_work |> 
  # General
  mutate(resolved_age_impute = ifelse(
    enpro_resolved_age %in% c("Reading Error", "No Age", ""), 
    NA, enpro_resolved_age),
    # Cowichan R Project errors - 2007
    resolved_age_impute = case_when(
      enpro_resolved_age == "01" & 
        enpro_year == 2007 & 
        enpro_project == "Cowichan River River Assessment" ~ "21",
      enpro_resolved_age == "02" & 
        enpro_year == 2007 & 
        enpro_project == "Cowichan River River Assessment" ~ "31",
      enpro_resolved_age == "03" & 
        enpro_year == 2007 & 
        enpro_project == "Cowichan River River Assessment" ~ "41",
      enpro_resolved_age == "03" &
        enpro_age_gr == "1M" & 
        enpro_year == 2007 & 
        enpro_project == "Cowichan River River Assessment" ~ "21",
      # Cowichan R Project errors - 2012
      enpro_resolved_age == "2" & 
        enpro_year == 2012 & 
        enpro_project == "Cowichan River River Assessment" ~ "21",
      enpro_resolved_age == "5" & 
        enpro_year == 2012 & 
        enpro_project == "Cowichan River River Assessment" ~ "51",
      # Cowichan R Project errors - 2015
      enpro_resolved_age == "3" & 
        enpro_year == 2015 & 
        enpro_project == "Cowichan River River Assessment" ~ "31",
      enpro_resolved_age == "4" & 
        enpro_year == 2015 & 
        enpro_project == "Cowichan River River Assessment" ~ "41",
      TRUE ~ resolved_age_impute))


#### Fill gaps in 'enpro_resolved_age' ----

##### Using 'enpro_age_gr' ----
sep_biodata_work$resolved_age_impute |> unique()
sep_biodata_work$enpro_age_gr |> unique()

sep_biodata_work <- sep_biodata_work |> 
  mutate(resolved_age_impute = ifelse(is.na(resolved_age_impute), 
                                      enpro_age_gr, 
                                      resolved_age_impute))

sep_biodata_work$resolved_age_impute |> unique() |> sort()

##### Using 'age_gr_est' ----
#' Estimate Gilbert-Rich age ('age_gr_est'):
#' 1. Estimate freshwater age
#' 2. Estimate CWT age
#' 3. Combine to create estimated Gilbert-Rich age

###### 1. Estimate freshwater age from 'enpro_smolt_or_yearling' ----
sep_biodata_work$enpro_smolt_or_yearling |> unique()

sep_biodata_work <- sep_biodata_work |> 
  mutate(age_fw_impute = ifelse(
    enpro_smolt_or_yearling %in% c("", "Error", "#N/A", "Missing Data"),
    NA, as.numeric(enpro_smolt_or_yearling)))

sep_biodata_work |> distinct(enpro_smolt_or_yearling, age_fw_impute)

###### 2. Estimate CWT age as difference between 'enpro_year' and 'enpro_brood_year' ----
sep_biodata_work$age_cwt <- sep_biodata_work$enpro_year - sep_biodata_work$enpro_brood_year

###### 3. Combine 'age_cwt' and 'age_fw_impute' to create 'age_gr_est' ----
sep_biodata_work <- sep_biodata_work |> 
  mutate(age_gr_est = ifelse(!is.na(age_cwt) & !is.na(age_fw_impute),
                          paste0(sep_biodata_work$age_cwt, sep_biodata_work$age_fw_impute),
                          NA))

sep_biodata_work$age_gr_est |> unique()

###### 4. Add 'age_gr_est' to 'resolved_age_impute' ----
sep_biodata_work <- sep_biodata_work |> 
  mutate(resolved_age_impute = ifelse(is.na(resolved_age_impute), 
                                      age_gr_est, resolved_age_impute))

sep_biodata_work$resolved_age_impute |> table(useNA = "ifany")

### Impute 'age_ocean' ----
sep_biodata_work <- sep_biodata_work |> 
  mutate(age_ocean = case_when(
    resolved_age_impute %in% c("21", "32", "43") ~ "1-ocean",
    resolved_age_impute %in% c("31", "42", "53") ~ "2-ocean",
    resolved_age_impute %in% c("41", "52", "63") ~ "3-ocean",
    resolved_age_impute %in% c("51", "62") ~ "4-ocean",
    resolved_age_impute == "61" ~ "5-ocean",
    resolved_age_impute == "71" ~ "6-ocean",
    TRUE ~ NA))

sep_biodata_work$age_ocean |> table(useNA = "ifany")
#' 1-ocean 2-ocean 3-ocean 4-ocean 5-ocean 6-ocean    <NA> 
#'   21166   52567   68741   21049     454       1   96304

### Corrections based on jack lengths
#' Jacks are male Chinook salmon that have returned to spawn after spending only
#' a year at sea. However, jacks that attain lengths > 500 mm are reclassified
#' here as adults, as *they do not have the same size limitations jacks*
#' *typically face when returning to spawn* 
#' *NEED TO CONFIRM EXPLANATION AND WHY ONLY 21 JACKS INCLUDED*.

sep_biodata_work <- sep_biodata_work |> 
  mutate(age_ocean = case_when(
    enpro_sex_final_impute == "Male" &
      enpro_post_orbital_hypural_poh < 500 ~ "1-ocean",
    enpro_sex_final_impute == "Male" & 
      age_ocean == "21" &
      enpro_post_orbital_hypural_poh > 500 ~ "2-ocean",
    TRUE ~ age_ocean))

sep_biodata_work$age_ocean |> table(useNA = "ifany")
#' 1-ocean 2-ocean 3-ocean 4-ocean 5-ocean 6-ocean    <NA> 
#'   21166   52567   68741   21049     454       1   96304 


### Impute 'sample_date' ----
sep_biodata_work$enpro_sample_start_date |> unique()

sep_biodata_work <- sep_biodata_work |> 
  mutate(sample_datetime_impute = ymd_hms(enpro_sample_start_date, tz = "UTC"),
         sample_date_impute = as.Date(sample_datetime_impute))

sep_biodata_work$sample_date_impute |> unique()


### Impute Sample Source ----
sep_biodata_work$enpro_sample_source1 |> unique()
sep_biodata_work$enpro_sample_source |> unique()
sep_biodata_work |> distinct(enpro_sample_source, enpro_sample_source1) |> 
  print(n = 100)


### Finalize table ----
# Select columns needed and label table
sep_biodata_final <- sep_biodata_work |>
  select(id, sample_date_impute, year = enpro_year, stock_impute, 
         enpro_site_river_location_impute, oto_stock_of_origin_impute, 
         sex_final_impute = enpro_sex_final_impute, 
         pohl = enpro_post_orbital_hypural_poh, age_ocean, 
         adipose_fin_clip_impute = enpro_adipose_fin_clip_impute, 
         final_use_distribution_impute = enpro_final_use_distribution) |> 
  mutate(tbl_name = "sep_biodata")

# Add to main database
sockeye <- list(sep_biodata = sep_biodata_final)

# Save files
dest <- "data/sockeye/SEP_BIODATA.csv"
write.csv(sep_biodata_final, "data-raw/sockeye/SEP_BIODATA.csv")
dest <- "data/sockeye/SEP_BIODATA.Rdata"
save(sockeye, file = dest)

# Remove unneeded variables
rm(enpro_work, oto_work, sep_biodata, sep_biodata_work, sep_biodata_final)

# Title: 01 - Data cleaning
# Author: Ravi Maharaj
# Date: 2025-07-30

# Description ----
#' This script is used to clean-up and standardize variables in different 
#' sources of biodata used in our analyses.

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

# Data cleaning ----

#------------------#
## SEP - ENPRO ----
#------------------#

## Load data
# enpro_dat <- readxl::read_xlsx("data/kokanee/SEP/SEP_ENPRO_CN.xlsx")
# enpro_meta <- readxl::read_xlsx("data/kokanee/SEP/SEP_ENPRO_CN_METADATA.xlsx")

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

## Load data
# oto_dat <- readxl::read_xlsx("data/kokanee/SEP/SEP_OTO_THERM_CN.xlsx")
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
#' 'resolved_age' contains many `NA` values and some errors in Cowichan R data.
#' First, erroneous data from Cowichan R were corrected, then `NA` values were
#' replaced with age values from 'age_gr' (Gilbert-Rich age) and an estimate of 
#' Gilbert-Rich age called 'age_cwt', created using 'brood_year' (year of 
#' release) and 'enpro_smolt_or_yearling' (life stage at release).

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

##### Using 'age_cwt' ----
#' Reconstruct 'age_cwt' (Gilbert-Rich age):
#' 1. Estimate freshwater age
#' 2. Estimate total age
#' 3. Combine into single value

###### 1. Estimate freshwater age from 'enpro_smolt_or_yearling' ----
sep_biodata_work$enpro_smolt_or_yearling |> unique()

sep_biodata_work <- sep_biodata_work |> 
  mutate(age_fw_impute = ifelse(
    enpro_smolt_or_yearling %in% c("", "Error", "#N/A", "Missing Data"),
    NA, as.numeric(enpro_smolt_or_yearling)))

sep_biodata_work |> distinct(enpro_smolt_or_yearling, age_fw_impute)

###### 2. Estimate Total age as difference between 'enpro_year' and 'enpro_brood_year' ----
sep_biodata_work$age_total_impute <- sep_biodata_work$enpro_year - sep_biodata_work$enpro_brood_year

###### 3. Combine 'age_total_impute' and 'age_fw_impute' to create 'age_cwt' ----
sep_biodata_work <- sep_biodata_work |> 
  mutate(age_cwt = ifelse(!is.na(age_total_impute) & !is.na(age_fw_impute),
                          paste0(sep_biodata_work$age_total_impute, sep_biodata_work$age_fw_impute),
                          NA))

sep_biodata_work$age_cwt |> unique()

###### 4. Add 'age_cwt_impute' to 'resolved_age_impute' ----
sep_biodata_work <- sep_biodata_work |> 
  mutate(resolved_age_impute = ifelse(is.na(resolved_age_impute), 
                            age_cwt, resolved_age_impute))

sep_biodata_work$resolved_age_impute |> table(useNA = "ifany")

### Impute 'ocean_age' ----
sep_biodata_work <- sep_biodata_work |> 
  mutate(ocean_age = case_when(
    resolved_age_impute %in% c("21", "32", "43") ~ "1-ocean",
    resolved_age_impute %in% c("31", "42", "53") ~ "2-ocean",
    resolved_age_impute %in% c("41", "52", "63") ~ "3-ocean",
    resolved_age_impute %in% c("51", "62") ~ "4-ocean",
    resolved_age_impute == "61" ~ "5-ocean",
    resolved_age_impute == "71" ~ "6-ocean",
    TRUE ~ NA))

sep_biodata_work$ocean_age |> table(useNA = "ifany")
#' 1-ocean 2-ocean 3-ocean 4-ocean 5-ocean 6-ocean    <NA> 
#'   21166   52567   68741   21049     454       1   96304

### Corrections based on jack lengths
#' Jacks are male Chinook salmon that have returned to spawn after spending only
#' a year at sea. However, jacks that attain lengths > 500 mm are reclassified
#' here as adults, as *they do not have the same size limitations jacks*
#' *typically face when returning to spawn* 
#' *NEED TO CONFIRM EXPLANATION AND WHY ONLY 21 JACKS INCLUDED*.

sep_biodata_work <- sep_biodata_work |> 
  mutate(ocean_age = case_when(
    enpro_sex_final_impute == "Male" &
      enpro_post_orbital_hypural_poh < 500 ~ "1-ocean",
    enpro_sex_final_impute == "Male" & 
      ocean_age == "21" &
      enpro_post_orbital_hypural_poh > 500 ~ "2-ocean",
    TRUE ~ ocean_age))

sep_biodata_work$ocean_age |> table(useNA = "ifany")
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
  select(id, sample_date_impute, 
         year = enpro_year, 
         stock_impute, enpro_site_river_location_impute, oto_stock_of_origin_impute, 
         sex_final_impute = enpro_sex_final_impute, 
         pohl = enpro_post_orbital_hypural_poh, ocean_age, 
         adipose_fin_clip_impute = enpro_adipose_fin_clip_impute, 
         final_use_distribution_impute = enpro_final_use_distribution) |> 
  mutate(tbl_name = "sep_biodata")

# Add to main database
sockeye <- list(sep_biodata = sep_biodata_final)

# Save .csv
write.csv(sep_biodata_final, "data-raw/sockeye/sep_biodata.csv")

# Remove unneeded variables
rm(enpro_work, oto_work, sep_biodata, sep_biodata_work, sep_biodata_final)


#-------------------------#
## Kitimat R Hatchery ----
#-------------------------#

## Load data ----
kit_dat <- read.csv("data/nonenpro/KitimatR.csv") |> 
  clean_names()
kit_dat2 <- read.csv("data/nonenpro/KitimatR_14_20.csv") |> 
  clean_names()

names(kit_dat)
names(kit_dat2)

# I added these myself
unique(kit_dat$id)
unique(kit_dat2$id)

## Impute 
kit_dat$Age <- as.character(kit_dat$Age)
kit_dat$POHL <- as.character(kit_dat$POHL)

kit_dat2 <- kit_dat2[,-which(is.na(names(kit_dat2)))]

kit_dat <- rbind.data.frame(kit_dat, kit_dat2)

# Postorbital-hypural length ####
sort(unique(kit_dat$POHL))
kit_dat$POHL_clean <- as.numeric(kit_dat$POHL)

# Sex ####
kit_dat$enpro_sex_final_impute[kit_dat$SEX...FINAL=="M"] <- "MALE"
kit_dat$enpro_sex_final_impute[kit_dat$SEX...FINAL=="F"] <- "FEMALE"
kit_dat$enpro_sex_final_impute[kit_dat$SEX...FINAL=="J"] <- "MALE"

# Ocean Age ####
kit_dat$ocean_age[kit_dat$Age=="2"] <- "1-ocean"
kit_dat$ocean_age[kit_dat$Age=="21"] <- "1-ocean"
kit_dat$ocean_age[kit_dat$Age=="32"] <- "1-ocean"
kit_dat$ocean_age[kit_dat$Age=="3"] <- "2-ocean"
kit_dat$ocean_age[kit_dat$Age=="31"] <- "2-ocean"
kit_dat$ocean_age[kit_dat$Age=="42"] <- "2-ocean"
kit_dat$ocean_age[kit_dat$Age=="4"] <- "3-ocean"
kit_dat$ocean_age[kit_dat$Age=="41"] <- "3-ocean"
kit_dat$ocean_age[kit_dat$Age=="52"] <- "3-ocean"
kit_dat$ocean_age[kit_dat$Age=="5"] <- "4-ocean"
kit_dat$ocean_age[kit_dat$Age=="51"] <- "4-ocean"
kit_dat$ocean_age[kit_dat$Age=="62"] <- "4-ocean"
kit_dat$ocean_age[kit_dat$Age=="6"] <- "5-ocean"
kit_dat$ocean_age[kit_dat$Age=="7"] <- "6-ocean"


kit_dat$ocean_age[kit_dat$enpro_sex_final_impute=="Male" &
                      kit_dat$POHL_clean<500] <- "1-ocean"
kit_dat$ocean_age[kit_dat$enpro_sex_final_impute=="Male" &
                      kit_dat$ocean_age=="21" &
                      kit_dat$POHL_clean>500] <- "2-ocean"

# Life stage ####
kit_dat$Life_Stage_clean[kit_dat$SEX...FINAL == "J"] <- "Jack"
kit_dat$Life_Stage_clean[kit_dat$ocean_age=="1-ocean" & kit_dat$enpro_sex_final_impute=="Male"] <- "Jack"
kit_dat$Life_Stage_clean[kit_dat$SEX...FINAL == "F"] <- "Adult"
kit_dat$Life_Stage_clean[kit_dat$SEX...FINAL == "M"] <- "Adult"

# Stock ####
kit_dat$enpro_site_river_location_impute <- "Kitimat R"
kit_dat$stock_impute <- kit_dat$enpro_site_river_location_impute

# Database ####
kit_dat$Database <- "Kitimat R Hatchery"

intersect(names(cnbiodata_clean), names(kit_dat))

df <- kit_dat[,names(cnbiodata_clean)]

if(out==T) {
  write.csv(kit_dat, "dat/processed/kit_clean.csv")
}

cnbiodata_clean <- cnbiodata_clean |>
  mutate(ID = as.character(ID)) |>
  add_row(kit_dat)




#------------------------------------------------------------------------------#
# NECHAKO RIVER - DFO ####
#------------------------------------------------------------------------------#

ndfo_dat <- read.csv("./data-raw/nonenpro/nechako_dfo.csv")

# Day, Month & Year ####

# A < 1000
# A = Month
# B = Day
# C = Year

# A > 1000
# A = Year
# B = Month
# C = Year

ndfo_dat <- ndfo_dat |>
  mutate(date_2 = date) |>
  separate(date_2, c("A", "B", "C"),
           sep ="[^[:alnum:]]+") |>
  mutate(Month = ifelse(A < 1000, A, B)) |>
  mutate(Day = ifelse(A < 1000, B, C)) |>
  mutate(Year = ifelse(A < 1000, C, A))

# Postorbital-hypural length ####
ndfo_dat <- ndfo_dat |>
  mutate(POHL_clean = POHL) |>
  mutate(POHL_clean = as.numeric(ifelse(POHL_clean == "n/a", NA, POHL_clean)))

# Sex ####
ndfo_dat$enpro_sex_final_impute <- ndfo_dat$sex
ndfo_dat$enpro_sex_final_impute[ndfo_dat$sex=="f"] <- "Female"
ndfo_dat$enpro_sex_final_impute[ndfo_dat$sex=="m"] <- "Male"
ndfo_dat$enpro_sex_final_impute[ndfo_dat$sex=="F"] <- "Female"
ndfo_dat$enpro_sex_final_impute[ndfo_dat$sex=="M"] <- "Male"
ndfo_dat$enpro_sex_final_impute[ndfo_dat$sex==""] <- NA

# Age ####
ndfo_dat$ocean_age[ndfo_dat$G.R=="32"] <- "1-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="43"] <- "1-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="31"] <- "2-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="42"] <- "2-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="53"] <- "2-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="41"] <- "3-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="52"] <- "3-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="63"] <- "3-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="51"] <- "4-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="62"] <- "4-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="73"] <- "4-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="61"] <- "5-ocean"
ndfo_dat$ocean_age[ndfo_dat$G.R=="72"] <- "5-ocean"

ndfo_dat$ocean_age[ndfo_dat$POHL_clean<500 & ndfo_dat$enpro_sex_final_impute=="Male"] <- "1-ocean"

ndfo_dat <- ndfo_dat |>
  mutate(ocean_age = ifelse(ocean_age=="1-ocean",
                           ifelse(enpro_sex_final_impute=="Male",
                                  ifelse(POHL_clean>500, "2-ocean", "1-ocean"),
                                  ocean_age), ocean_age))

# Stock ####
ndfo_dat$SampleSite_clean <- ndfo_dat$Stock
ndfo_dat$Stock_clean <- ndfo_dat$Stock
ndfo_dat$Stock_clean2 <- ndfo_dat$Stock

# Database ####
ndfo_dat$Database <- "Nechako R (DFO)"

if(out==T) {
  write.csv(ndfo_dat, "dat/processed/ndfo_clean.csv")
}

# Join with rest of biodata
temp <- ndfo_dat |>
  mutate(Year = as.numeric(Year)) |>
  select(ID, Year, SampleSite_clean, Stock_clean, Stock_clean2, 
         enpro_sex_final_impute, POHL_clean, ocean_age, Database)

cnbiodata_clean <- cnbiodata_clean |>
  mutate(ID = as.character(ID)) |>
  add_row(temp)




#------------------------------------------------------------------------------#
# ROBERTSON CR HISTORICAL DATA ####
#------------------------------------------------------------------------------#

rh_dat <- read.csv("./data-raw/historical/robertson_compiled.csv")

# Postorbital-hypural length ####
rh_dat <- rh_dat |>
  filter(!LENGTH.C.9==0) |>
  mutate(LENGTH.C.9 = ifelse(LENGTH.C.9=="N/A", NA, LENGTH.C.9)) |>
  mutate(Length_clean = as.integer(LENGTH.C.9)) |>
  mutate(Length_clean = ifelse(YEAR %in% c(1990:1994), Length_clean*10, Length_clean)) |>
  mutate(POHL_clean = ifelse(LM.C.9 %in% c("1", "POH"), Length_clean, NA)) 

nrow(rh_dat)
# [1] 22647

# Sex ####
# Based on sex codes
rh_dat$enpro_sex_final_impute[rh_dat$SEX.C.9==2] <- "Female"
rh_dat$enpro_sex_final_impute[rh_dat$SEX.C.9==1] <- "Male"
rh_dat$enpro_sex_final_impute[rh_dat$SEX.C.9==3] <- "Male"
rh_dat$enpro_sex_final_impute[rh_dat$SEX.C.9=="M"] <- "Male"
rh_dat$enpro_sex_final_impute[rh_dat$SEX.C.9=="F"] <- "Female"
rh_dat$enpro_sex_final_impute[rh_dat$SEX.C.9=="J"] <- "Male"


# Ocean Age ####
rh_dat$ocean_age <- NA
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="2"] <- "1-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="21"] <- "1-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="32"] <- "1-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="43"] <- "1-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="3"] <- "2-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="31"] <- "2-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="42"] <- "2-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="53"] <- "2-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="4"] <- "3-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="41"] <- "3-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="52"] <- "3-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="5"] <- "4-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="51"] <- "4-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="6"] <- "5-ocean"
rh_dat$ocean_age[rh_dat$S_AGE.C.9=="61"] <- "5-ocean"

rh_dat <- rh_dat |>
  mutate(ocean_age = ifelse(ocean_age=="1-ocean",
                           ifelse(enpro_sex_final_impute=="Male",
                                  ifelse(POHL_clean>500, "2-ocean", "1-ocean"),
                                  ocean_age), ocean_age))

# Fill gaps in ocean age based on CWT age, where no scale age data present.
# Leave out instances where CWT age==1, not interested in fish where total age
# suggests they have not spent time in the ocean.

# clean up CWT Age
sort(unique(rh_dat$age_cwt.C.9))
# Some are numerically compatible...
# "6", "5", "4", "3", "2", "1", "1988", "1987", "1986", "1989", "1990"
# These contain CWT ages and brood years. CWT ages will be used as they are, brood years will be used to
# calculate CWT age.

# Some are not...
# "", "*1", "    ", "  ", "1984/4", "1985/3", "UNKNWN", "1986/2", "1982/6", "FALL 87/2", "UNKNOWN", "FALL 86/3",
#  "FALL 85/4", "FALL 84/5", "FALL86/4", "FALL88/2", "FALL85/5", "FALL87/3", "FALL84/6", "FALL 86/4",
# "FALL 87/3", "FALL 88/2", "FALL 85/5", "UNKNOWN ", "MYSTERY", "S", "RG"
# These contain CWT age and brood years separated by '/'s. Need to be split into columns with constituent 
# parts and CWT age conditionally extracted from each column.

nrow(rh_dat)
# [1] 25220

# Take numerically compatible entries. All text entries become NA.
temp <- rh_dat |>
  mutate(age_cwt_clean=as.numeric(age_cwt.C.9))

unique(temp$age_cwt)
# Calculate CWT age using brood year entries.
temp2 <- temp |>
  filter(age_cwt_clean>1000)
unique(temp2$age_cwt_clean)
temp2$age_cwt_clean <- temp2$enpro_year - temp2$age_cwt_clean
unique(temp2$age_cwt_clean)
# Get entries with CWT age and bind rows with calculated CWT age.
temp2 <- temp |>
  filter(!age_cwt_clean>1000) |>
  bind_rows(temp2)
unique(temp2$age_cwt_clean)
# Recombine numerically compatible/incompatible entries to reform full data-frame
temp2 <- temp |>
  filter(is.na(age_cwt_clean)) |>
  bind_rows(temp2)
unique(temp2$age_cwt_clean)
nrow(temp2)
# [1] 22647

# Take rows with numerically incompatible entries from this data-frame. Split entries into constituent parts 
# and select values for CWT age (<80)
temp <- temp2 |>
  mutate(A = as.numeric(age_cwt.C.9)) |>
  filter(is.na(A)) |>
  separate(age_cwt.C.9, c("B", "C", "D"),
           sep ="[^[:alnum:]]+", remove = F) |>
  mutate(age_cwt_clean = as.numeric(ifelse(C<80, C,
                                           ifelse(D<80, D, NA)))) |>
  select(-A, -B, -C, -D)
unique(temp$age_cwt_clean)
nrow(temp)

# Select numerically compatible entries and recombine with newly cleaned 'age_cwt_clean' variable
temp2 <- temp2 |>
  mutate(A = as.numeric(age_cwt.C.9)) |>
  filter(!is.na(A)) |>
  select(-A) |>
  bind_rows(temp)
nrow(temp2)
# [1] 22647
unique(temp2$age_cwt_clean)

rh_dat <- temp2

# Check that age_cwt_clean matches respective values in age_cwt.C.9
rh_dat |>
  select(age_cwt_clean, age_cwt.C.9, YEAR) |>
  distinct()

# Select rows with no entries under ocean age and add based on age_cwt_clean (CWT age - 1).

# Select rows with no entries under ocean age, estimate GR age using 
# age_cwt_clean and FW age. Where no FW age, assume as 1.
temp <- rh_dat |>
  filter(is.na(ocean_age)) |>
  unite("GR_age", age_cwt_clean, FW.Age, sep="", remove=F)

temp$ocean_age[temp$GR_age=="2"] <- "1-ocean"
temp$ocean_age[temp$GR_age=="21"] <- "1-ocean"
temp$ocean_age[temp$GR_age=="32"] <- "1-ocean"
temp$ocean_age[temp$GR_age=="43"] <- "1-ocean"
temp$ocean_age[temp$GR_age=="3"] <- "2-ocean"
temp$ocean_age[temp$GR_age=="31"] <- "2-ocean"
temp$ocean_age[temp$GR_age=="42"] <- "2-ocean"
temp$ocean_age[temp$GR_age=="53"] <- "2-ocean"
temp$ocean_age[temp$GR_age=="4"] <- "3-ocean"
temp$ocean_age[temp$GR_age=="41"] <- "3-ocean"
temp$ocean_age[temp$GR_age=="5"] <- "4-ocean"
temp$ocean_age[temp$GR_age=="51"] <- "4-ocean"
temp$ocean_age[temp$GR_age=="6"] <- "5-ocean"
temp$ocean_age[temp$GR_age=="61"] <- "5-ocean"

# Select rows with ocean age from scale age and add ocean age from CWT age.
temp2 <- rh_dat |>
  filter(!is.na(ocean_age)) |>
  bind_rows(temp)
nrow(temp2)
x <- temp2 |>
  filter(is.na(ocean_age))
nrow(x)
# Corrections to ocean age based on length
temp2$ocean_age[temp2$POHL_clean>500 & temp2$ocean_age=="1-ocean" & temp2$enpro_sex_final_impute=="Male"] <- "2-ocean"
temp2$ocean_age[temp2$POHL_clean<501 & temp2$enpro_sex_final_impute=="Male"] <- "1-ocean"

# Check corresponding entries of GR_age & S_AGE.C.9 (are there 
# inconsistencies where both are present?)
temp2 |>
  filter(!is.na(age_cwt_clean)) |>
  select(ocean_age, S_AGE.C.9, GR_age, age_cwt_clean, FW.Age) |>
  distinct()

rh_dat <- temp2

# Life stage ####
# Based on sex codes
unique(rh_dat$SEX.C.9)
# "1", "2", "3", "M", "F", "J", "UNKNWN", "?", "", " ", "u", "UNKNOWN"

rh_dat$LifeStage_clean[rh_dat$SEX.C.9==1] <- "Adult"
rh_dat$LifeStage_clean[rh_dat$SEX.C.9==2] <- "Adult"
rh_dat$LifeStage_clean[rh_dat$SEX.C.9==3] <- "Jack"
rh_dat$LifeStage_clean[rh_dat$SEX.C.9=="M"] <- "Adult"
rh_dat$LifeStage_clean[rh_dat$SEX.C.9=="F"] <- "Adult"
rh_dat$LifeStage_clean[rh_dat$SEX.C.9=="J"] <- "Jack"
# Corrections based on length
rh_dat |>
  mutate(LifeStage_clean = ifelse(enpro_sex_final_impute=="Male",
                                  ifelse(POHL_clean<501, "Jack", "Adult"), "Adult"))
rh_dat$LifeStage_clean[rh_dat$POHL_clean>500] <- "Adult"

# Check consistency in lengths across age and life stage
rh_dat |>
  filter(LifeStage_clean=="Jack") |>
  select(POHL_clean) |>
  distinct() |>
  arrange(POHL_clean)

# Area - Final Use ####
rh_dat$FinalUse_clean <- NA

# Stock ####
rh_dat$SampleSite_clean <- "Robertson Cr"
rh_dat$Stock_clean <- "Robertson Cr"
rh_dat$Stock_clean2 <- "Robertson Cr"

# Database ####
rh_dat$Database <- "SEP Historical"

# Year
rh_dat$enpro_year <- rh_dat$enpro_year

# Out
if(out==T) {
  write.csv(rh_dat, "dat/processed/rh_clean.csv")
}

# Select columns
temp <- rh_dat |>
  select(ID, Year, Stock_clean, Stock_clean2, SampleSite_clean,
         enpro_sex_final_impute, POHL_clean, ocean_age, Database)

cnbiodata_clean <- cnbiodata_clean |>
  mutate(ID = as.character(ID)) |>
  add_row(temp)




#------------------------------------------------------------------------------#
# CHILLIWACK R HISTORICAL DATA ####
#------------------------------------------------------------------------------#

ch_dat <- read.csv("./data-raw/historical/chilliwack_compiled.csv")

# Postorbital-hypural length ####
ch_dat <- ch_dat |>
  mutate(Length_clean = as.numeric(LENGTH.C.9)) |>
  filter(!Length_clean==0) |>
  mutate(POHL_clean = ifelse(LM.C.9==1, Length_clean, NA)) 
# |>
#   filter(!is.na(POHL_clean))

ch_dat$enpro_year <- ch_dat$enpro_year

ch_dat |>
  group_by(LM.C.9) |>
  summarise(n())

# Sex ####
# Based on sex codes
ch_dat$enpro_sex_final_impute[ch_dat$SEX.C.9==2] <- "Female"
ch_dat$enpro_sex_final_impute[ch_dat$SEX.C.9==1] <- "Male"
ch_dat$enpro_sex_final_impute[ch_dat$SEX.C.9==3] <- "Male"

# Ocean age ####
# From scale age
ch_dat$ocean_age <- NA
ch_dat$ocean_age[ch_dat$S_AGE.C.9=="21"] <- "1-ocean"
ch_dat$ocean_age[ch_dat$S_AGE.C.9=="32"] <- "1-ocean"
ch_dat$ocean_age[ch_dat$S_AGE.C.9=="31"] <- "2-ocean"
ch_dat$ocean_age[ch_dat$S_AGE.C.9=="42"] <- "2-ocean"
ch_dat$ocean_age[ch_dat$S_AGE.C.9=="41"] <- "3-ocean"
ch_dat$ocean_age[ch_dat$S_AGE.C.9=="51"] <- "4-ocean"

# Check number of rows with ocean age
n <- ch_dat |>
  filter(!is.na(ocean_age))
nrow(n)
# 622

# Fill gaps in ocean age with CWT age; Select rows that don't have scale age
# and add ocean age
temp <- ch_dat |>
  filter(is.na(ocean_age))
temp$ocean_age[temp$age_cwt.C.9=="21"] <- "1-ocean"
temp$ocean_age[temp$age_cwt.C.9=="2"] <- "1-ocean"
temp$ocean_age[temp$age_cwt.C.9=="31"] <- "2-ocean"
temp$ocean_age[temp$age_cwt.C.9=="3"] <- "2-ocean"
temp$ocean_age[temp$age_cwt.C.9=="41"] <- "3-ocean"
temp$ocean_age[temp$age_cwt.C.9=="4"] <- "3-ocean"
temp$ocean_age[temp$age_cwt.C.9=="51"] <- "4-ocean"
temp$ocean_age[temp$age_cwt.C.9=="5"] <- "4-ocean"

# Check number of rows with ocean age from CWT age
n <- temp |>
  filter(!is.na(ocean_age))
nrow(n)
# 205

# Select rows with ocean age from scale age and recombine with rows lacking
# scale age and back-filled with CWT age
temp <- ch_dat |>
  filter(!is.na(ocean_age)) |>
  bind_rows(temp)

# Check number of rows with ocean age
n <- temp |>
  filter(!is.na(ocean_age))
nrow(n)
# 827

ch_dat <- temp

ch_dat <- ch_dat |>
  mutate(ocean_age = ifelse(ocean_age=="1-ocean",
                           ifelse(enpro_sex_final_impute=="Male",
                                  ifelse(POHL_clean>500, "2-ocean", "1-ocean"),
                                  ocean_age), ocean_age))

# Stock ####
ch_dat$Stock_clean <- "Chilliwack R"
ch_dat$Stock_clean2 <- "Chilliwack R"
ch_dat$SampleSite_clean <- "Chilliwack R"

# Database ####
ch_dat$Database <- "SEP Historical"

if(out==T) {
  write.csv(ch_dat, "dat/processed/ch_clean.csv")
}

# Select columns
temp <- ch_dat |>
  select(ID, Year, Stock_clean, Stock_clean2, SampleSite_clean,
         enpro_sex_final_impute, POHL_clean, ocean_age, Database) |>
  filter(!Year==1990)

cnbiodata_clean <- cnbiodata_clean |>
  mutate(ID = as.character(ID)) |>
  add_row(temp)


#------------------------------------------------------------------------------#
# RMIS ####

# Load releases
rls <- read.csv("./data-raw/rmis/rls_50_21.csv")

# Load recovery files
fns <- list.files("./data-raw/rmis/rcv/")
fns <- fns[str_detect(fns, "rcv")]
fns <- fns[str_detect(fns, "csv")]

# Compile recovery files into one data-frame
rcv <- data.frame()
i=1
for(i in 1:length(fns)) {
  fn <- fns[i]
  temp <- read.csv(paste("./data-raw/rmis/rcv/",fn,"", sep=""))

  rcv <- rbind(rcv, temp)
}

# Select Chinook records
rcv2 <- rcv |>
  filter(species==1) |>
  filter(!tag_code=="")

unique(rcv2$enpro_year)

rcv2$Month <- substr(rcv2$recovery_date, 5, 6)

rls2 <- rls |>
  filter(species==1) |>
  filter(!tag_code_or_release_id=="") |>
  rename(tag_code = tag_code_or_release_id) |>
  group_by(tag_code) |>
  select(tag_code, tag_type, species, run, brood_year,
         last_release_date, release_location_code, release_location_name,
         hatchery_location_code, hatchery_location_name, stock_location_code,
         stock_location_name, release_stage)

rls2 |>
  group_by(release_stage) |>
  summarise(n())

rls2$release_year <- substr(rls2$last_release_date, 1, 4)

# Combine rls & rcv

rmis <- rcv2 |>
  group_by(tag_code) |>
  select(tag_code, tag_type, species, run_year,
         fishery, gear, sex, weight, weight_code,
         weight_type, length, length_code, length_type) |>
  left_join(rls2) |>
  ungroup()

# Postorbital-hypural length ####
rmis$POHL_clean <- rmis$length
rmis <- rmis |>
  filter(!is.na(POHL_clean))

# Sex ####
rmis$enpro_sex_final_impute <- NA
rmis$enpro_sex_final_impute[rmis$sex=="M"] <- "Male"
rmis$enpro_sex_final_impute[rmis$sex=="F"] <- "Female"
rmis$enpro_sex_final_impute[rmis$sex==""] <- NA

# Ocean age ####
# Estimate GR age using release year, run year and release stage

# Infer freshwater age from release stage
rmis$fw_age <- NA
rmis$fw_age[rmis$release_stage=="E"] <- 1
rmis$fw_age[rmis$release_stage=="F"] <- 1
rmis$fw_age[rmis$release_stage=="P"] <- 1
rmis$fw_age[rmis$release_stage=="S"] <- 1
rmis$fw_age[rmis$release_stage=="Y"] <- 2

# Calculate cwt age from run year and brood year and combine with freshwater
# age to give Gilbert-rich age
rmis <- rmis |>
  mutate(age_cwt = as.integer(run_year) - as.integer(brood_year)) |>
  filter(!is.na(age_cwt)) |>
  filter(!is.na(fw_age)) |>
  unite("gr_age", age_cwt, fw_age, sep="", remove = F)

rmis$ocean_age[rmis$gr_age=="21"] <- "1-ocean"
rmis$ocean_age[rmis$gr_age=="32"] <- "1-ocean"
rmis$ocean_age[rmis$gr_age=="43"] <- "1-ocean"
rmis$ocean_age[rmis$gr_age=="31"] <- "2-ocean"
rmis$ocean_age[rmis$gr_age=="42"] <- "2-ocean"
rmis$ocean_age[rmis$gr_age=="53"] <- "2-ocean"
rmis$ocean_age[rmis$gr_age=="41"] <- "3-ocean"
rmis$ocean_age[rmis$gr_age=="52"] <- "3-ocean"
rmis$ocean_age[rmis$gr_age=="51"] <- "4-ocean"
rmis$ocean_age[rmis$gr_age=="62"] <- "4-ocean"
rmis$ocean_age[rmis$gr_age=="61"] <- "5-ocean"
rmis$ocean_age[rmis$gr_age=="72"] <- "5-ocean"
rmis$ocean_age[rmis$gr_age=="71"] <- "6-ocean"
rmis$ocean_age[rmis$gr_age=="82"] <- "6-ocean"

# Corrections based on length
rmis$ocean_age[rmis$POHL_clean<501 & rmis$enpro_sex_final_impute=="Male"] <- "1-ocean"
rmis$ocean_age[rmis$POHL_clean>500 & rmis$enpro_sex_final_impute=="Male" & rmis$ocean_age=="1-ocean"] <- "2-ocean"

# Remove 6-ocean fish due to inconsistencies in length distribution
rmis <- rmis |>
  filter(!ocean_age=="6-ocean")

# Stock ####
nrow(rmis)
temp <- rmis$release_location_name
length(temp)
temp <- substr(temp, 3, 100)
length(temp)
rmis$Release_site <- temp
sort(unique(rmis$Release_site))
# [1] "Adams R Up"         "Alberni In"         "Alice Lk Outlet"    "Anweiler Cr"        "Ashlu Cr"
# [6] "Atnarko Ch"         "Atnarko R"          "Atnarko R Low"      "Atnarko R Up"       "Babine R"
# [11] "Babine R Up"        "Baezaeko R"         "Barkley Sd"         "Beaver Cv"          "Bedwell Est"
# [16] "Bedwell R"          "Benson R"           "Big Qualicum R"     "Billy Harris Sl"    "Birkenhead R"
# [21] "Bonaparte R"        "Bonaparte R Low"    "Bonaparte R Up"     "Bowron R"           "Brandon Is"
# [26] "Bridge R"           "Bulkley R Up"       "Buntzen Bay"        "Burman R"           "Burrard In"
# [31] "Campbell Est"       "Campbell R"         "Campbell Transitn"  "Capilano R"         "Capilano R Up"
# [36] "Cariboo R Low"      "Cariboo R Up"       "Cedar R"            "Cheakamus R"        "Chehalis R"
# [41] "Chemainus R"        "Chilcotin R"        "Chilko R"           "Chilliwack R"       "China Cr"
# [46] "China Est"          "Chuckwalla R"       "Clearwater R Low"   "Clearwater R Up"    "Coldwater R"
# [51] "Comox Lk"           "Conuma Est"         "Conuma R"           "Courtenay Est"      "Cowichan Est"
# [56] "Cowichan Lk"        "Cowichan R"         "Cowichan R Low"     "Cowichan R Up"      "Cowichan R@Duncan"
# [61] "Cruickshank R"      "Dala R"             "Date Cr+Kispiox R"  "Deadman R"          "Deep Bay/GSMN"
# [66] "Deep Bay/GSVI"      "Deep Cr/SKNA"       "Deep Cv"            "Deepwater Bay"      "Deer Bay"
# [71] "Devereux Cr"        "Discovery Pass"     "Dome Cr"            "Drew Hb"            "Eagle R"
# [76] "Egmont Point"       "Eliza Pass"         "Elk Falls Ch #1"    "Englishman R"       "Englishman R S"
# [81] "Erlandsen Cr"       "Fairy Lk"           "False Cr"           "Fanny Bay/JNST"     "Fee Ch"
# [86] "Ferguson Swamp"     "Fidalgo Pass"       "Finn Cr"            "First Lk/GSVI"      "Fontoniko Cr"
# [91] "Fulton R"           "Fulton+Babine R"    "Gold R"             "Goldstream Est"     "Grief Point"
# [96] "Hadenschild Cr"     "Harbour Quay"       "Hardy Bay"          "Harris Cr/SWVI"     "Harrison R"
# [101] "Hicks Cr"           "Hirsch Cr"          "Horsefly R"         "Horseshoe Bay"      "Hurry Cr"
# [106] "Indian Arm"         "Indian R"           "Indianpoint Cr"     "Ioco"               "Jack Point"
# [111] "James Cr"           "Jitco Cr"           "Kanyon Cr"          "Kathleen Lk"        "Kennedy Lk"
# [116] "Kennedy R Low"      "Kilbella Bay"       "Kilbella R"         "Kildala R"          "Kincolith R"
# [121] "King Cr Low/QCI"    "King Cr Up/QCI"     "Kispiox R"          "Kispiox R Tribs"    "Kitimat R"
# [126] "Kitimat R Low"      "Kitimat R Up"       "Kitsum Abv Canyon"  "Kitsum Bel Canyon"  "Kitsumkalum R"
# [131] "Kokish Est"         "Kokish R"           "Koksilah R"         "L Nitinat R"        "L Qualicum R"
# [136] "Lang Cr"            "Lean-To Cr"         "Leiner R"           "Link R"             "Little R/UPFR"
# [141] "Lonespoon Cr"       "MacBlo/English Ch"  "Maclean Bay"        "Mamquam R"          "Marble R"
# [146] "Maria Sl"           "Marie Lk"           "McKinley Cr"        "Michie Cr"          "Mill Bay"
# [151] "Millstream Est"     "Morice R"           "Moutcha Bay"        "Muchalat Lk"        "Nahmint Est"
# [156] "Nahmint R"          "Nakina R"           "Nanaimo R"          "Napoleon Cr"        "Nazko R"
# [161] "Neechanze R"        "Nicola R"           "Nicola R Up"        "Nimpkish Lk"        "Nimpkish R"
# [166] "Nimpkish R Low"     "Nitinat Lk"         "Nitinat R"          "Nusatsum R"         "Orange Pt"
# [171] "Oweekeeno Lk"       "Oyster R"           "Pallant Cr"         "Phillips Lk"        "Phillips R"
# [176] "Pitt R Up"          "Poett Nook"         "Port Renfrew"       "Porteau Cv"         "Puntledge R"
# [181] "Puntledge R Up"     "Quatse R"           "Quatsino Sd"        "Quesnel Lk"         "Quesnel R"
# [186] "Quesnel Townsite"   "Quinsam Lk"         "Quinsam Lk Up"      "Quinsam R"          "Quinsam R Up"
# [191] "Raft R"             "Rainy Bay"          "Reed Point"         "Rivers In"          "Robertson Cr"
# [196] "Saanich In"         "Salmon R/JNST"      "Salmon R/TOMF"      "Salmon R/UPFR"      "Saloompt R"
# [201] "San Juan R"         "Sandy Cv"           "Sarita R"           "Seal Bay"           "Seymour R/GSMN"
# [206] "Shovelnose Cr"      "Shuswap R Low"      "Shuswap R Middle"   "Skutz Falls"        "Sliammon R"
# [211] "Slim Cr"            "Sooke Hb"           "Sooke R"            "Spius Cr"           "Squamish Est"
# [216] "Squamish R"         "Stamp R"            "Stave R"            "Sterling Cr"        "Stuart Lk"
# [221] "Stuart R"           "Sucwoa R"           "Swift R/UPFR"       "Tahsis R"           "Taku Lodge"
# [226] "Tenderfoot Cr"      "Tenderfoot Lk"      "Thompson R N"       "Thornton Cr"        "Thornton Est"
# [231] "Tlupana R"          "Toquart Est"        "Toquart Lk"         "Toquart R"          "Tranquil Cr"
# [236] "Tranquil Est"       "Vancouver R"        "Vernon Lk"          "Wagidis Ch"         "Wannock Est"
# [241] "Wannock R"          "West Road R"        "Willow Cr/JNST"     "Willow R/UPFR"      "Woss Lk"
# [246] "Yakoun Est"         "Yakoun R"           "Zymoetz (Copper) R"

# Combine Stocks
rmis$Release_site[rmis$Release_site=="Atnarko R Low"] <- "Atnarko R"
rmis$Release_site[rmis$Release_site=="Atnarko R Up"] <- "Atnarko R"
rmis$Release_site[rmis$Release_site=="First Lk/GSVI"] <- "Nanaimo R"
rmis$Release_site[rmis$Release_site=="Hirsch Cr"] <- "Kitimat R"
rmis$Release_site[rmis$Release_site=="Kitimat R Low"] <- "Kitimat R"
rmis$Release_site[rmis$Release_site=="Kitimat R Up"] <- "Kitimat R"
rmis$Release_site[rmis$Release_site=="Cowichan Lk"] <- "Cowichan R"
rmis$Release_site[rmis$Release_site=="Cowichan R Low"] <- "Cowichan R"
rmis$Release_site[rmis$Release_site=="Cowichan R Up"] <- "Cowichan R"
rmis$Release_site[rmis$Release_site=="Cowichan R@Duncan"] <- "Cowichan R"
rmis$Release_site[rmis$Release_site=="Cowichan Est"] <- "Cowichan R"
rmis$Release_site[rmis$Release_site=="Quinsam Lk"] <- "Quinsam R"
rmis$Release_site[rmis$Release_site=="Quinsam Lk Up"] <- "Quinsam R"
rmis$Release_site[rmis$Release_site=="Quinsam R Up"] <- "Quinsam R"
rmis$Release_site[rmis$Release_site=="Nitinat Lk"] <- "Nitinat R"
rmis$Release_site[rmis$Release_site=="Conuma Est"] <- "Conuma R"
rmis$Release_site[rmis$Release_site=="Capilano R Up"] <- "Capilano R"
rmis$Release_site[rmis$Release_site=="Bedwell Est"] <- "Bedwell R"
rmis$Release_site[rmis$Release_site=="Puntledge R Up"] <- "Puntledge R"
rmis$Release_site[rmis$Release_site=="Shuswap R Low"] <- "Shuswap R"
rmis$Release_site[rmis$Release_site=="Shuswap R Middle"] <- "Shuswap R"
rmis$Release_site[rmis$Release_site=="Kitsum Abv Canyon"] <- "Kitsumkalum R"
rmis$Release_site[rmis$Release_site=="Kitsum Bel Canyon"] <- "Kitsumkalum R"
rmis$Stock_clean <- rmis$Release_site
rmis$Stock_clean2 <- rmis$Release_site



# Database ####
rmis$Database <- "RMIS"

# ID ####
rmis$A <- "RMIS"
rmis$B <- c(1:nrow(rmis))
rmis <- rmis |>
  unite("ID", A, B, sep="")

# Year ####
rmis$enpro_year <- rmis$run_year

if(out==T) {
  write.csv(rmis, "dat/processed/rmis_clean.csv")
}

temp <- rmis |>
  select(ID, Year, Stock_clean, Stock_clean2, Release_site,
         enpro_sex_final_impute, POHL_clean, ocean_age, Database)

cnbiodata_clean <- cnbiodata_clean |>
  mutate(ID = as.character(ID)) |>
  add_row(temp)




#------------------------------------------------------------------------------#
# COMPLETE DATABASE ####
#------------------------------------------------------------------------------#

if(out==T) {
  write.csv(cnbiodata_clean, "dat/processed/cnbiodata_clean.csv")
}


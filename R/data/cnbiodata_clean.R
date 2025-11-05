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

# Load data ----
# load("./data-raw/raw_data.Rdata")

# Data cleaning ----

## SEP data - ENPRO ----
## Load data
# enpro_work <- readxl::read_xlsx("data-raw/enpro/Kent_AllChinook_BiodataDB.xlsx")
# cols_def <- readxl::read_xlsx("data-raw/meta/biodata_column_meanings.xlsx")

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
    TRUE ~ site_river_location_impute))

sort(unique(enpro_work$site_river_location_impute))
rm(dupes_enpro, keep_rows, enpro_dupe_ids)

## SEP data - ERIN ----
## Load data
# erin_dat <- readxl::read_xlsx("data-raw/meta/erin_thermalmark.xlsx")
erin_work <- erin_dat |> clean_names()

### Check for dupes ----
dupes_erin <- erin_work |> 
  select(id, stock_of_origin) |> 
  filter(!is.na(id)) |> 
  janitor::get_dupes()

### Impute stock_of_origin ----
sort(unique(erin_work$stock_of_origin))

#### Consolidate values in stock_of_origin
#' Convert all "" values to NA
#' Combine "Atnarko R Low", "Atnarko R Up" & "Atnarko R" into "Atnarko R"
#' Combine "First Lk/GSVI" & "Nanaimo R" into "Namaimo R"
erin_work <- erin_work |> 
  mutate(stock_of_origin_impute = str_trim(stock_of_origin),
         stock_of_origin_impute = case_when(
           stock_of_origin_impute == "First Lk/GSVI" ~ "Nanaimo R",
           stock_of_origin_impute %in% c("Atnarko R Low", "Atnarko R Up") ~ "Atnarko R",
           stock_of_origin_impute == "" ~ NA,
           TRUE ~ stock_of_origin_impute
         ))

erin_work |> distinct(stock_of_origin, stock_of_origin_impute) |> 
  mutate(diff = ifelse(stock_of_origin_impute != stock_of_origin, T, F)) |> 
  filter(diff == T)
# print(n=100)


#### Reduce ERIN to its unique variables and foreign key 'id'
names_enpro <- names(enpro_work)
names_erin <- names(erin_work)
match_names <- intersect(names_enpro, names_erin)
setdiff(names_enpro, match_names)
#' ERIN contains all but 7 of the columns from ENPRO

#' Tag all untagged ENPRO variables
names_enpro <- purrr::modify(names_enpro, \(x) {
  if(!grepl('enpro', x) & x != 'id') {
    paste0('enpro_', x)
  } else {
    x
  }
})

#' Tag variables unique to ERIN
names_erin <- purrr::modify(names_erin, \(x) {
  if(!x %in% match_names) {
    paste0('erin_', x)
  } else {
    x
  }
})

#' Check for correct order of variable names, order correct if output is ""
modify2(names_enpro, names(enpro_work), \(x, y) {
  z <- gsub(y, "", x)
  gsub("enpro_", "", z)
}) |> unique()
modify2(names_erin, names(erin_work), \(x, y) {
  z <- gsub(y, "", x)
  gsub("erin_", "", z)
}) |> unique()

#' Set names
names(enpro_work) <- names_enpro
names(erin_work) <- names_erin

#' Select unique columns for ERIN with foreign key
names_erin <- c("id", names_erin[grepl("erin_", names_erin)])
erin_work <- erin_work |> 
  select(all_of(names_erin))
names(erin_work)


## SEP Biodata ----
### Create SEP Biodata table ----
#' Join ERIN and ENPRO tables to get common observations
sep_biodata <- erin_work |>
  right_join(enpro_work, 
             by = "id")

### Clear unneeded variables
rm(dupes_erin, match_names, names_enpro, names_erin)

### Impute 'stock' ----
#' Use `stock_of_origin_impute` with NA values replaced with those from 
#' `site_river_location_impute`.
#' 
#' After standardizing both for uniformity below: 
#' - *~60%* of observations have `NA` values for `stock_of_origin_impute`
#' - The correspondence rate between `stock_of_origin_impute` and 
#'   `site_river_location_impute` is *~94.8%*

#### Check percent of observations with NA stock_of_origin_impute
sep_biodata |> 
  group_by(stock_of_origin_impute) |> 
  summarise(n_obs_stk = n()) |> 
  ungroup() |> 
  mutate(n_obs_all = sum(n_obs_stk),
         p_obs_stk = 100*(n_obs_stk/n_obs_all)) |> 
  arrange(desc(p_obs_stk)) |> 
  print(n = 100)
#' ~60% of observations have NA values for stock_of_origin

#### Standardize values for `stock_of_origin_impute` & `site_river_location_impute`
sep_biodata_work <- sep_biodata_work |> 
  #' `stock_of_origin_impute`
  mutate(stock_of_origin_impute = case_when(grepl("Qual", stock_of_origin_impute) ~ "B+L Qualicum R",
                                            grepl("Capilano\\+Chill", stock_of_origin_impute) ~ "Capilano R",
                                            grepl("Chill\\+Harrison R", stock_of_origin_impute) ~ "Chilliwack R",
                                            grepl("Salmon R/JNST", stock_of_origin_impute) ~ "Salmon R",
                                            TRUE ~ stock_of_origin_impute),
  #' `site_river_location_impute`
         site_river_location_impute = case_when(grepl("Qual", site_river_location_impute) ~ "B+L Qualicum R",
                                                grepl("Nanaimo R Low Fall", site_river_location_impute) ~ "Nanaimo R",
                                                grepl("Nanaimo R Up Summer", site_river_location_impute) ~ "Nanaimo R",
                                                grepl("Great Central Lake Fishway", site_river_location_impute) ~ "Robertson Cr",
                                            TRUE ~ site_river_location_impute))


#### Check correspondence between stock_of_origin_impute and site_river_location_impute
check_correspondance <- sep_biodata_work |> 
  select(stock_of_origin_impute, site_river_location_impute) |> 
  filter(!is.na(stock_of_origin_impute)) |> 
  mutate(diff = ifelse(stock_of_origin_impute == site_river_location_impute,
                       "same", "diff")) #|> 
  # distinct() |> filter(diff == T)

check_correspondance |>
  group_by(diff) |> 
  mutate(n_pair = n()) |> ungroup() |>
  distinct(diff, n_pair) |> 
  pivot_wider(values_from = n_pair, names_from = diff) |> 
  mutate(p_diff = 100*(diff/sum(same, diff)))
#' `stock_of_origin_impute` and `site_river_location_impute` differ in ~5.19% of 
#' observations with values for both variables.

#' Assuming that all fish will return to their home waterbody, location where 
#' fish were sampled, `site_river_location_impute`, used to fill `NA` values in
#' `stock_of_origin_impute` to create `stock_impute`.
sep_biodata_work <- sep_biodata_work |>
  mutate(stock_impute_flag = ifelse(is.na(stock_of_origin_impute),
                                       "imputed",
                                       "actual"),
         stock_impute = ifelse(is.na(stock_of_origin_impute),
                                        site_river_location_impute, 
                                        stock_of_origin_impute))

sep_biodata_work |> 
  filter(is.na(stock_impute)) |> nrow()
sep_biodata_work$stock_impute_flag |> table()

### Impute final_use_distribution ----
unique(sep_biodata_work$final_use_distribution)

sep_biodata_work <- sep_biodata_work |> 
  mutate(final_use_distribution_impute = case_when(
    final_use_distribution == "Natural Spawners" ~ "Natural Spawner",
    final_use_distribution == "Not Applicable" ~ NA,
    final_use_distribution == "Unknown" ~ NA,
    TRUE ~ final_use_distribution
  ))


### Impute post_orbital_hypural_poh ----
unique(sep_biodata_work$post_orbital_hypural_poh)
typeof(sep_biodata_work$post_orbital_hypural_poh)
sort(unique(sep_biodata_work$post_orbital_hypural_poh))


### Impute ocean_age ----
#' "ocean_age" estimated using the following protocol:
#' 1. Fix errors in resolved_age for Cowichan R data
#' 2. Fill gaps in resolved_age with age_gr (Gilbert-rich)
#' 3. Estimate freshwater age using sep_biodata_worka on life stage at release provided by ENPRO (SMOLT OR YearLING)
#' 4. Estimate total age as the difference between brood Year and sample Year
#' 5. Combine total and freshwater age to produce CWT age and clean up to remove erroneous results
#' 6. Backfill resolved age with CWT age
#' 7. clean up ocean age

# 1. clean up Resolved Age
# General
sep_biodata_work$Resolved_Age_clean <- sep_biodata_work$`RESOLVED AGE`
sort(unique(sep_biodata_work$Resolved_Age_clean))
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="Reading Error"] <- NA
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="No Age"] <- NA
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`==""] <- NA

# Cowichan R Proj Age Errors
sep_biodata_work$Year_clean <- sep_biodata_work$YEAR
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="01" &
                                sep_biodata_work$Year_clean==2007 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "21"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="02" &
                                sep_biodata_work$Year_clean==2007 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "31"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="03" &
                                sep_biodata_work$Year_clean==2007 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "41"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="03" &
                                sep_biodata_work$Year_clean==2007 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment" &
                                sep_biodata_work$AGE_GR=="1M"] <- "21"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="2" &
                                sep_biodata_work$Year_clean==2012 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "21"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="3" &
                                sep_biodata_work$Year_clean==2015 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "31"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="4" &
                                sep_biodata_work$Year_clean==2015 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "41"
sep_biodata_work$Resolved_Age_clean[sep_biodata_work$`RESOLVED AGE`=="5" &
                                sep_biodata_work$Year_clean==2012 &
                                sep_biodata_work$PROJECT=="Cowichan River River Assessment"] <- "51"

# 2. Fill gaps in Resolved Age with Gilbert Rich Ages from PADs
sep_biodata_work$Age_GR_clean <- sep_biodata_work$AGE_GR
sort(unique(sep_biodata_work$Age_GR_clean))
sep_biodata_work$Age_R_GR <- ifelse(is.na(sep_biodata_work$Resolved_Age_clean), 
                             sep_biodata_work$Age_GR_clean, sep_biodata_work$Resolved_Age_clean)
sort(unique(sep_biodata_work$Age_R_GR))

unique(sep_biodata_work[,c("Age_GR_clean", "Resolved_Age_clean", "Age_R_GR")]) |> 
  print(n=96)

# 3. Estimate freshwater age
# clean up ENPRO: Smolt or Yearling (Freshwater age)
sep_biodata_work$Smolt_Yearling_clean <- sep_biodata_work$`ENPRO: SMOLT OR YEARLING`
unique(sep_biodata_work$Smolt_Yearling_clean)
sep_biodata_work$Smolt_Yearling_clean[sep_biodata_work$`ENPRO: SMOLT OR YEARLING`==""] <- NA
sep_biodata_work$Smolt_Yearling_clean[sep_biodata_work$`ENPRO: SMOLT OR YEARLING`=="Error"] <- NA
sep_biodata_work$Smolt_Yearling_clean[sep_biodata_work$`ENPRO: SMOLT OR YEARLING`=="#N/A"] <- NA
sep_biodata_work$Smolt_Yearling_clean[sep_biodata_work$`ENPRO: SMOLT OR YEARLING`=="Missing Data"] <- NA
sep_biodata_work$Smolt_Yearling_clean <- as.numeric(sep_biodata_work$Smolt_Yearling_clean)
unique(sep_biodata_work$Smolt_Yearling_clean)

# 4. Estimate Total age
sep_biodata_work$Total_Age <- sep_biodata_work$Year_clean - sep_biodata_work$BROOD_YEAR

# 5. Combine freshwater and total age to estimate CWT age, clean up result and backfill resolved age with CWT age
sep_biodata_work$CWT_Age <- paste0(sep_biodata_work$Total_Age, sep_biodata_work$Smolt_Yearling_clean)
unique(sep_biodata_work$CWT_Age)
sep_biodata_work$CWT_Age <- ifelse(grepl("NA", sep_biodata_work$CWT_Age), NA, sep_biodata_work$CWT_Age)
unique(sep_biodata_work$CWT_Age)

sep_biodata_work$Ocean_Age <- ifelse(is.na(sep_biodata_work$Age_R_GR), 
                             sep_biodata_work$CWT_Age, sep_biodata_work$Age_R_GR)
unique(sep_biodata_work$Ocean_Age)

check_age <- sep_biodata_work[,c("CWT_Age", "Age_R_GR", "Ocean_Age")] |> 
  unique()
check_age$check_age <- ifelse(check_age$CWT_Age == check_age$Ocean_Age &
                                check_age$CWT_Age != check_age$Age_R_GR,
                              T, F)

# 7. clean up Ocean Age
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="21"] <- "Ocean-1"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="32"] <- "Ocean-1"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="43"] <- "Ocean-1"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="31"] <- "Ocean-2"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="42"] <- "Ocean-2"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="53"] <- "Ocean-2"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="41"] <- "Ocean-3"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="52"] <- "Ocean-3"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="63"] <- "Ocean-3"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="51"] <- "Ocean-4"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="62"] <- "Ocean-4"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="61"] <- "Ocean-5"
sep_biodata_work$Ocean_Age[sep_biodata_work$Ocean_Age=="71"] <- "Ocean-6"

# Corrections based on jack lengths
sep_biodata_work$Ocean_Age[sep_biodata_work$sex_final_impute=="Male" &
                        sep_biodata_work$POHL_clean<500] <- "Ocean-1"
sep_biodata_work$Ocean_Age[sep_biodata_work$sex_final_impute=="Male" &
                        sep_biodata_work$Ocean_Age=="21" &
                        sep_biodata_work$POHL_clean>500] <- "Ocean-2"


# Impute Date -------------------------------------------------------------------------
unique(sep_biodata_work$SAMPLE_START_DATE)
sep_biodata_work$Sample_Date_Start_clean <- sapply(sep_biodata_work$SAMPLE_START_DATE, function(x) {
  strsplit(x, "T")[[1]][1]
})
sep_biodata_work$Sample_Date_Start_clean <- as.Date(sep_biodata_work$Sample_Date_Start_clean)
sep_biodata_work$Month <- lubridate::month(sep_biodata_work$Sample_Date_Start_clean)


# Impute Sample Source ####
sep_biodata_work$Sample_Source_clean <- sep_biodata_work$SAMPLE_SOURCE1
unique(sep_biodata_work$Sample_Source_clean)

# Database ####
sep_biodata_work$Database <- "EnPro"


# Choose columns ####
cnbiodata_clean <- sep_biodata_work |>
  select(ID, Sample_Date_Start_clean, Month, Year_clean, site_river_location_impute, 
         stock_impute, sex_final_impute, POHL_clean, Ocean_Age, 
         Database)


write.csv(sep_biodata_work, "data-raw/clean/enpro_clean.csv")

rm(sep_biodata_work)


#------------------------------------------------------------------------------#
# KITIMAT R HATCHERY ####
#------------------------------------------------------------------------------#

# Data ####
kit_dat <- read.csv("data-raw/nonenpro/KitimatR.csv")
kit_dat2 <- read.csv("data-raw/nonenpro/KitimatR_14_20.csv")

names(kit_dat)
names(kit_dat2)

# I added these myself
unique(kit_dat$ID)
unique(kit_dat2$ID)

colnames(kit_dat) <- c("ID", "Year_clean", "site_river_location_impute", "Tag_code",
                    "SEX...FINAL", "Age", "POHL")

colnames(kit_dat2) <- c("ID", "Year_clean", "site_river_location_impute", "Tag_code",
                     "SEX...FINAL", "Age", "POHL")

kit_dat$Age <- as.character(kit_dat$Age)
kit_dat$POHL <- as.character(kit_dat$POHL)

kit_dat2 <- kit_dat2[,-which(is.na(names(kit_dat2)))]

kit_dat <- rbind.data.frame(kit_dat, kit_dat2)

# Postorbital-hypural length ####
sort(unique(kit_dat$POHL))
kit_dat$POHL_clean <- as.numeric(kit_dat$POHL)

# Sex ####
kit_dat$sex_final_impute[kit_dat$SEX...FINAL=="M"] <- "MALE"
kit_dat$sex_final_impute[kit_dat$SEX...FINAL=="F"] <- "FEMALE"
kit_dat$sex_final_impute[kit_dat$SEX...FINAL=="J"] <- "MALE"

# Ocean Age ####
kit_dat$Ocean_Age[kit_dat$Age=="2"] <- "Ocean-1"
kit_dat$Ocean_Age[kit_dat$Age=="21"] <- "Ocean-1"
kit_dat$Ocean_Age[kit_dat$Age=="32"] <- "Ocean-1"
kit_dat$Ocean_Age[kit_dat$Age=="3"] <- "Ocean-2"
kit_dat$Ocean_Age[kit_dat$Age=="31"] <- "Ocean-2"
kit_dat$Ocean_Age[kit_dat$Age=="42"] <- "Ocean-2"
kit_dat$Ocean_Age[kit_dat$Age=="4"] <- "Ocean-3"
kit_dat$Ocean_Age[kit_dat$Age=="41"] <- "Ocean-3"
kit_dat$Ocean_Age[kit_dat$Age=="52"] <- "Ocean-3"
kit_dat$Ocean_Age[kit_dat$Age=="5"] <- "Ocean-4"
kit_dat$Ocean_Age[kit_dat$Age=="51"] <- "Ocean-4"
kit_dat$Ocean_Age[kit_dat$Age=="62"] <- "Ocean-4"
kit_dat$Ocean_Age[kit_dat$Age=="6"] <- "Ocean-5"
kit_dat$Ocean_Age[kit_dat$Age=="7"] <- "Ocean-6"


kit_dat$Ocean_Age[kit_dat$sex_final_impute=="Male" &
                      kit_dat$POHL_clean<500] <- "Ocean-1"
kit_dat$Ocean_Age[kit_dat$sex_final_impute=="Male" &
                      kit_dat$Ocean_Age=="21" &
                      kit_dat$POHL_clean>500] <- "Ocean-2"

# Life stage ####
kit_dat$Life_Stage_clean[kit_dat$SEX...FINAL == "J"] <- "Jack"
kit_dat$Life_Stage_clean[kit_dat$Ocean_Age=="Ocean-1" & kit_dat$sex_final_impute=="Male"] <- "Jack"
kit_dat$Life_Stage_clean[kit_dat$SEX...FINAL == "F"] <- "Adult"
kit_dat$Life_Stage_clean[kit_dat$SEX...FINAL == "M"] <- "Adult"

# Stock ####
kit_dat$site_river_location_impute <- "Kitimat R"
kit_dat$stock_impute <- kit_dat$site_river_location_impute

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
ndfo_dat$sex_final_impute <- ndfo_dat$sex
ndfo_dat$sex_final_impute[ndfo_dat$sex=="f"] <- "Female"
ndfo_dat$sex_final_impute[ndfo_dat$sex=="m"] <- "Male"
ndfo_dat$sex_final_impute[ndfo_dat$sex=="F"] <- "Female"
ndfo_dat$sex_final_impute[ndfo_dat$sex=="M"] <- "Male"
ndfo_dat$sex_final_impute[ndfo_dat$sex==""] <- NA

# Age ####
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="32"] <- "Ocean-1"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="43"] <- "Ocean-1"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="31"] <- "Ocean-2"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="42"] <- "Ocean-2"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="53"] <- "Ocean-2"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="41"] <- "Ocean-3"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="52"] <- "Ocean-3"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="63"] <- "Ocean-3"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="51"] <- "Ocean-4"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="62"] <- "Ocean-4"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="73"] <- "Ocean-4"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="61"] <- "Ocean-5"
ndfo_dat$Ocean_Age[ndfo_dat$G.R=="72"] <- "Ocean-5"

ndfo_dat$Ocean_Age[ndfo_dat$POHL_clean<500 & ndfo_dat$sex_final_impute=="Male"] <- "Ocean-1"

ndfo_dat <- ndfo_dat |>
  mutate(Ocean_Age = ifelse(Ocean_Age=="Ocean-1",
                           ifelse(sex_final_impute=="Male",
                                  ifelse(POHL_clean>500, "Ocean-2", "Ocean-1"),
                                  Ocean_Age), Ocean_Age))

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
         sex_final_impute, POHL_clean, Ocean_Age, Database)

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
rh_dat$sex_final_impute[rh_dat$SEX.C.9==2] <- "Female"
rh_dat$sex_final_impute[rh_dat$SEX.C.9==1] <- "Male"
rh_dat$sex_final_impute[rh_dat$SEX.C.9==3] <- "Male"
rh_dat$sex_final_impute[rh_dat$SEX.C.9=="M"] <- "Male"
rh_dat$sex_final_impute[rh_dat$SEX.C.9=="F"] <- "Female"
rh_dat$sex_final_impute[rh_dat$SEX.C.9=="J"] <- "Male"


# Ocean Age ####
rh_dat$Ocean_Age <- NA
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="2"] <- "Ocean-1"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="21"] <- "Ocean-1"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="32"] <- "Ocean-1"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="43"] <- "Ocean-1"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="3"] <- "Ocean-2"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="31"] <- "Ocean-2"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="42"] <- "Ocean-2"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="53"] <- "Ocean-2"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="4"] <- "Ocean-3"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="41"] <- "Ocean-3"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="52"] <- "Ocean-3"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="5"] <- "Ocean-4"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="51"] <- "Ocean-4"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="6"] <- "Ocean-5"
rh_dat$Ocean_Age[rh_dat$S_AGE.C.9=="61"] <- "Ocean-5"

rh_dat <- rh_dat |>
  mutate(Ocean_Age = ifelse(Ocean_Age=="Ocean-1",
                           ifelse(sex_final_impute=="Male",
                                  ifelse(POHL_clean>500, "Ocean-2", "Ocean-1"),
                                  Ocean_Age), Ocean_Age))

# Fill gaps in ocean age based on CWT age, where no scale age data present.
# Leave out instances where CWT age==1, not interested in fish where total age
# suggests they have not spent time in the ocean.

# clean up CWT Age
sort(unique(rh_dat$CWT_AGE.C.9))
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
  mutate(CWT_Age_clean=as.numeric(CWT_AGE.C.9))

unique(temp$CWT_Age)
# Calculate CWT age using brood year entries.
temp2 <- temp |>
  filter(CWT_Age_clean>1000)
unique(temp2$CWT_Age_clean)
temp2$CWT_Age_clean <- temp2$YEAR - temp2$CWT_Age_clean
unique(temp2$CWT_Age_clean)
# Get entries with CWT age and bind rows with calculated CWT age.
temp2 <- temp |>
  filter(!CWT_Age_clean>1000) |>
  bind_rows(temp2)
unique(temp2$CWT_Age_clean)
# Recombine numerically compatible/incompatible entries to reform full data-frame
temp2 <- temp |>
  filter(is.na(CWT_Age_clean)) |>
  bind_rows(temp2)
unique(temp2$CWT_Age_clean)
nrow(temp2)
# [1] 22647

# Take rows with numerically incompatible entries from this data-frame. Split entries into constituent parts 
# and select values for CWT age (<80)
temp <- temp2 |>
  mutate(A = as.numeric(CWT_AGE.C.9)) |>
  filter(is.na(A)) |>
  separate(CWT_AGE.C.9, c("B", "C", "D"),
           sep ="[^[:alnum:]]+", remove = F) |>
  mutate(CWT_Age_clean = as.numeric(ifelse(C<80, C,
                                           ifelse(D<80, D, NA)))) |>
  select(-A, -B, -C, -D)
unique(temp$CWT_Age_clean)
nrow(temp)

# Select numerically compatible entries and recombine with newly cleaned 'CWT_Age_clean' variable
temp2 <- temp2 |>
  mutate(A = as.numeric(CWT_AGE.C.9)) |>
  filter(!is.na(A)) |>
  select(-A) |>
  bind_rows(temp)
nrow(temp2)
# [1] 22647
unique(temp2$CWT_Age_clean)

rh_dat <- temp2

# Check that CWT_age_clean matches respective values in CWT_AGE.C.9
rh_dat |>
  select(CWT_Age_clean, CWT_AGE.C.9, YEAR) |>
  distinct()

# Select rows with no entries under ocean age and add based on CWT_age_clean (CWT age - 1).

# Select rows with no entries under ocean age, estimate GR age using 
# CWT_age_clean and FW age. Where no FW age, assume as 1.
temp <- rh_dat |>
  filter(is.na(Ocean_Age)) |>
  unite("GR_age", CWT_Age_clean, FW.Age, sep="", remove=F)

temp$Ocean_Age[temp$GR_age=="2"] <- "Ocean-1"
temp$Ocean_Age[temp$GR_age=="21"] <- "Ocean-1"
temp$Ocean_Age[temp$GR_age=="32"] <- "Ocean-1"
temp$Ocean_Age[temp$GR_age=="43"] <- "Ocean-1"
temp$Ocean_Age[temp$GR_age=="3"] <- "Ocean-2"
temp$Ocean_Age[temp$GR_age=="31"] <- "Ocean-2"
temp$Ocean_Age[temp$GR_age=="42"] <- "Ocean-2"
temp$Ocean_Age[temp$GR_age=="53"] <- "Ocean-2"
temp$Ocean_Age[temp$GR_age=="4"] <- "Ocean-3"
temp$Ocean_Age[temp$GR_age=="41"] <- "Ocean-3"
temp$Ocean_Age[temp$GR_age=="5"] <- "Ocean-4"
temp$Ocean_Age[temp$GR_age=="51"] <- "Ocean-4"
temp$Ocean_Age[temp$GR_age=="6"] <- "Ocean-5"
temp$Ocean_Age[temp$GR_age=="61"] <- "Ocean-5"

# Select rows with ocean age from scale age and add ocean age from CWT age.
temp2 <- rh_dat |>
  filter(!is.na(Ocean_Age)) |>
  bind_rows(temp)
nrow(temp2)
x <- temp2 |>
  filter(is.na(Ocean_Age))
nrow(x)
# Corrections to ocean age based on length
temp2$Ocean_Age[temp2$POHL_clean>500 & temp2$Ocean_Age=="Ocean-1" & temp2$sex_final_impute=="Male"] <- "Ocean-2"
temp2$Ocean_Age[temp2$POHL_clean<501 & temp2$sex_final_impute=="Male"] <- "Ocean-1"

# Check corresponding entries of GR_age & S_AGE.C.9 (are there 
# inconsistencies where both are present?)
temp2 |>
  filter(!is.na(CWT_Age_clean)) |>
  select(Ocean_Age, S_AGE.C.9, GR_age, CWT_Age_clean, FW.Age) |>
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
  mutate(LifeStage_clean = ifelse(sex_final_impute=="Male",
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
rh_dat$Year <- rh_dat$YEAR

# Out
if(out==T) {
  write.csv(rh_dat, "dat/processed/rh_clean.csv")
}

# Select columns
temp <- rh_dat |>
  select(ID, Year, Stock_clean, Stock_clean2, SampleSite_clean,
         sex_final_impute, POHL_clean, Ocean_Age, Database)

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

ch_dat$Year <- ch_dat$YEAR

ch_dat |>
  group_by(LM.C.9) |>
  summarise(n())

# Sex ####
# Based on sex codes
ch_dat$sex_final_impute[ch_dat$SEX.C.9==2] <- "Female"
ch_dat$sex_final_impute[ch_dat$SEX.C.9==1] <- "Male"
ch_dat$sex_final_impute[ch_dat$SEX.C.9==3] <- "Male"

# Ocean age ####
# From scale age
ch_dat$Ocean_Age <- NA
ch_dat$Ocean_Age[ch_dat$S_AGE.C.9=="21"] <- "Ocean-1"
ch_dat$Ocean_Age[ch_dat$S_AGE.C.9=="32"] <- "Ocean-1"
ch_dat$Ocean_Age[ch_dat$S_AGE.C.9=="31"] <- "Ocean-2"
ch_dat$Ocean_Age[ch_dat$S_AGE.C.9=="42"] <- "Ocean-2"
ch_dat$Ocean_Age[ch_dat$S_AGE.C.9=="41"] <- "Ocean-3"
ch_dat$Ocean_Age[ch_dat$S_AGE.C.9=="51"] <- "Ocean-4"

# Check number of rows with ocean age
n <- ch_dat |>
  filter(!is.na(Ocean_Age))
nrow(n)
# 622

# Fill gaps in ocean age with CWT age; Select rows that don't have scale age
# and add ocean age
temp <- ch_dat |>
  filter(is.na(Ocean_Age))
temp$Ocean_Age[temp$CWT_AGE.C.9=="21"] <- "Ocean-1"
temp$Ocean_Age[temp$CWT_AGE.C.9=="2"] <- "Ocean-1"
temp$Ocean_Age[temp$CWT_AGE.C.9=="31"] <- "Ocean-2"
temp$Ocean_Age[temp$CWT_AGE.C.9=="3"] <- "Ocean-2"
temp$Ocean_Age[temp$CWT_AGE.C.9=="41"] <- "Ocean-3"
temp$Ocean_Age[temp$CWT_AGE.C.9=="4"] <- "Ocean-3"
temp$Ocean_Age[temp$CWT_AGE.C.9=="51"] <- "Ocean-4"
temp$Ocean_Age[temp$CWT_AGE.C.9=="5"] <- "Ocean-4"

# Check number of rows with ocean age from CWT age
n <- temp |>
  filter(!is.na(Ocean_Age))
nrow(n)
# 205

# Select rows with ocean age from scale age and recombine with rows lacking
# scale age and back-filled with CWT age
temp <- ch_dat |>
  filter(!is.na(Ocean_Age)) |>
  bind_rows(temp)

# Check number of rows with ocean age
n <- temp |>
  filter(!is.na(Ocean_Age))
nrow(n)
# 827

ch_dat <- temp

ch_dat <- ch_dat |>
  mutate(Ocean_Age = ifelse(Ocean_Age=="Ocean-1",
                           ifelse(sex_final_impute=="Male",
                                  ifelse(POHL_clean>500, "Ocean-2", "Ocean-1"),
                                  Ocean_Age), Ocean_Age))

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
         sex_final_impute, POHL_clean, Ocean_Age, Database) |>
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

unique(rcv2$Year)

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
rmis$sex_final_impute <- NA
rmis$sex_final_impute[rmis$sex=="M"] <- "Male"
rmis$sex_final_impute[rmis$sex=="F"] <- "Female"
rmis$sex_final_impute[rmis$sex==""] <- NA

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
  mutate(cwt_age = as.integer(run_year) - as.integer(brood_year)) |>
  filter(!is.na(cwt_age)) |>
  filter(!is.na(fw_age)) |>
  unite("gr_age", cwt_age, fw_age, sep="", remove = F)

rmis$Ocean_Age[rmis$gr_age=="21"] <- "Ocean-1"
rmis$Ocean_Age[rmis$gr_age=="32"] <- "Ocean-1"
rmis$Ocean_Age[rmis$gr_age=="43"] <- "Ocean-1"
rmis$Ocean_Age[rmis$gr_age=="31"] <- "Ocean-2"
rmis$Ocean_Age[rmis$gr_age=="42"] <- "Ocean-2"
rmis$Ocean_Age[rmis$gr_age=="53"] <- "Ocean-2"
rmis$Ocean_Age[rmis$gr_age=="41"] <- "Ocean-3"
rmis$Ocean_Age[rmis$gr_age=="52"] <- "Ocean-3"
rmis$Ocean_Age[rmis$gr_age=="51"] <- "Ocean-4"
rmis$Ocean_Age[rmis$gr_age=="62"] <- "Ocean-4"
rmis$Ocean_Age[rmis$gr_age=="61"] <- "Ocean-5"
rmis$Ocean_Age[rmis$gr_age=="72"] <- "Ocean-5"
rmis$Ocean_Age[rmis$gr_age=="71"] <- "Ocean-6"
rmis$Ocean_Age[rmis$gr_age=="82"] <- "Ocean-6"

# Corrections based on length
rmis$Ocean_Age[rmis$POHL_clean<501 & rmis$sex_final_impute=="Male"] <- "Ocean-1"
rmis$Ocean_Age[rmis$POHL_clean>500 & rmis$sex_final_impute=="Male" & rmis$Ocean_Age=="Ocean-1"] <- "Ocean-2"

# Remove Ocean-6 fish due to inconsistencies in length distribution
rmis <- rmis |>
  filter(!Ocean_Age=="Ocean-6")

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
rmis$Year <- rmis$run_year

if(out==T) {
  write.csv(rmis, "dat/processed/rmis_clean.csv")
}

temp <- rmis |>
  select(ID, Year, Stock_clean, Stock_clean2, Release_site,
         sex_final_impute, POHL_clean, Ocean_Age, Database)

cnbiodata_clean <- cnbiodata_clean |>
  mutate(ID = as.character(ID)) |>
  add_row(temp)




#------------------------------------------------------------------------------#
# COMPLETE DATABASE ####
#------------------------------------------------------------------------------#

if(out==T) {
  write.csv(cnbiodata_clean, "dat/processed/cnbiodata_clean.csv")
}


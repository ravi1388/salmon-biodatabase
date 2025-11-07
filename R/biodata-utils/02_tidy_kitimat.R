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

# Load data ----
# load("./data/kokanee/kokanee.Rdata")

path <- "data/kokanee/KITIMAT/KITIMAT_CN_1.csv"
kit_dat <- read.csv(path) |>
  clean_names()
path <- "data/kokanee/KITIMAT/KITIMAT_CN_2.csv"
kit_dat2 <- read.csv(path) |>
  clean_names()

# Data cleaning ----

names(kit_dat)
names(kit_dat2)

str(kit_dat)
str(kit_dat2)


#--------------#
## kit_dat ----
#--------------#

### Impute 'age_ocean' ----
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

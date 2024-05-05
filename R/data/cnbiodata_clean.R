# Notes ####
# Use this script to gather, standardize and combine various biodata datasets
# used in our analyses

# Packages ####
library(tidyr)
library(dplyr)
library(magrittr)


# Variables ####
out <- F




#------------------------------------------------------------------------------#
# EnPro ####
#------------------------------------------------------------------------------#

# Data ####
enpro_dat <- readxl::read_xlsx("data-raw/enpro/Kent_AllChinook_BiodataDB.xlsx", 
                               n_max = 100)
# cols_def <- readxl::read_xlsx("data-raw/meta/biodata_column_meanings.xlsx")
erin_dat <- readxl::read_xlsx("data-raw/meta/erin_thermalmark.xlsx")

# Adipose fin clip ####
enpro_dat %>% 
  mutate(`ADIPOSE FIN CLIP` = tolower(`ADIPOSE FIN CLIP`)) %>% 
  pull(`ADIPOSE FIN CLIP`) %>% 
  unique()

enpro_dat <- enpro_dat %>% 
  mutate(adipose_fin_clip_clean = tolower(`ADIPOSE FIN CLIP`),
         adipose_fin_clip_clean = case_when(
           adipose_fin_clip_clean %in% c("ad", "yes", "y", "adlv", "adrv", "rv") ~ "Yes",
           is.na(adipose_fin_clip_clean) ~ NA,
           adipose_fin_clip_clean == "unch" ~ `ADIPOSE FIN CLIP`,
           TRUE ~ "No")) 



# Sex ####
enpro_dat %>% 
  mutate(`SEX - FINAL` = tolower(`SEX - FINAL`)) %>% 
  pull(`SEX - FINAL`) %>% unique()

enpro_dat <- enpro_dat %>% 
  mutate(sex_clean = tolower(`SEX - FINAL`),
         sex_clean = case_when(sex_clean == "male" ~ "Male",
                               sex_clean == "female" ~ "Female",
                               sex_clean == "u/k" ~ "UNKNOWN",
                               TRUE ~ toupper(sex_clean)))

enpro_dat %>% 
  distinct(sex_clean, `SEX - FINAL`)

enpro_dat$Sex_Clean <- enpro_dat$SEX...FINAL
enpro_dat$Sex_Clean[enpro_dat$Sex_Clean=="female"] <- "Female"
enpro_dat$Sex_Clean[enpro_dat$Sex_Clean=="male"] <- "Male"

# Year ####
enpro_dat$Year <- enpro_dat$YEAR


# Stock ####
enpro_dat <- erin_dat %>%
  select(ID, StockOfOrigin) %>%
  right_join(enpro_dat)

enpro_dat$StockOfOrigin[enpro_dat$StockOfOrigin==""] <- NA
enpro_dat$StockOfOrigin <- str_trim(enpro_dat$StockOfOrigin)
# Combined First Lake enpro_data with Nanaimo R
enpro_dat$StockOfOrigin[enpro_dat$StockOfOrigin=="First Lk/GSVI"] <- "Nanaimo R"

# Combine Atnarko R Low & Up
enpro_dat$StockOfOrigin[enpro_dat$StockOfOrigin=="Atnarko R Low"] <- "Atnarko R"
enpro_dat$StockOfOrigin[enpro_dat$StockOfOrigin=="Atnarko R Up"] <- "Atnarko R"

enpro_dat$Release_site <- enpro_dat$StockOfOrigin

# Standardize sample Site
enpro_dat$SampleSite_Clean <- enpro_dat$SITE.RIVER.LOCATION
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Atnarko R Low"] <- "Atnarko R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Atnarko R Mid"] <- "Atnarko R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Atnarko R Up"] <- "Atnarko R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Burman River"] <- "Burman R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="BURMAN RIVER"] <- "Burman R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Coldwater River"] <- "Coldwater R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="COLONIAL CREEK"] <- "Colonial Cr"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Conuma River"] <- "Conuma R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="CONUMA RIVER"] <- "Conuma R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Cowichan River"] <- "Cowichan R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="CYPRE RIVER"] <- "Cypre R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Gold River"] <- "Gold R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="GOLD RIVER"] <- "Gold R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="GODSPEED RIVER"] <- "Godspeed R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Leiner River"] <- "Leiner R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="LEINER RIVER"] <- "Leiner R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Klinaklini River"] <- "Klinaklini R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="MACLEAN BAY"] <- "Maclean B"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Marble River"] <- "Marble R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Menzies Creek"] <- "Menzies Cr"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Moheya River"] <- "Moheya R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="MOHEYA RIVER"] <- "Moheya R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="MUCHALAT RIVER"] <- "Muchalat R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Nimpkish River"] <- "Nimpkish R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="NITINAT RIVER"] <- "Nitinat R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="San Juan River"] <- "San Juan R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SAN JUAN RIVER"] <- "San Juan R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SARITA RIVER"] <- "Sarita R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SOMASS RIVER"] <- "Somass R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SOOKE RIVER"] <- "Sooke R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SOOKE RIVER"] <- "Sooke R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Spius Creek"] <- "Spius Cr"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Sproat River"] <- "Sproat R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SPROAT RIVER"] <- "Sproat R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="SUCWOA RIVER"] <- "Sucwoa R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Tahsis River"] <- "Tahsis R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="TAHSIS RIVER"] <- "Tahsis R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Tahsish River"] <- "Tahsis R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="THORNTON CREEK"] <- "Thornton Cr"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="TOQUART RIVER"] <- "Toquart R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Tranquil Creek"] <- "Tranquil Cr"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="TRANQUIL CREEK"] <- "Tranquil Cr"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="Zeballos River"] <- "Zeballos R"
enpro_dat$SampleSite_Clean[enpro_dat$SampleSite_Clean=="ZEBALLOS RIVER"] <- "Zeballos R"

# Fill gaps in Release_site with Sample site
enpro_dat <- enpro_dat %>%
  mutate(Stock_Clean = Release_site) %>%
  mutate(Stock_Clean2 = Release_site) %>%
  mutate(Stock_Clean2 = ifelse(is.na(Stock_Clean), SampleSite_Clean, Stock_Clean))


# Final Use ####
enpro_dat$FinalUse_Clean <- enpro_dat$FINAL.USE.DISTRIBUTION
enpro_dat$FinalUse_Clean[enpro_dat$FinalUse == "Natural Spawner"] <- "Natural Spawners"
enpro_dat$FinalUse_Clean[enpro_dat$FinalUse == "Not Applicable"] <- NA


# Postorbital-Hypural Length ####
enpro_dat$POHL_Clean <- as.numeric(enpro_dat$POST.ORBITAL.HYPURAL..POH.)


# Ocean Age ####
# Here we estimate a 'ocean age' using Jeffrey Till's 'Resolved Age' as a base
# 1. Clean up Cowichan R Age Errors in Resolved Age
# 2. Backfill resolved age with Gilbert-rich age
# 3. Estimate freshwater age using enpro_data on life stage at release provided by ENPRO (SMOLT OR YearLING)
# 4. Estimate total age as the difference between brood Year and sample Year
# 5. Combine total and freshwater age to produce CWT age and clean up to remove erroneous results
# 6. Backfill resolved age with CWT age
# 7. Clean up ocean age

# 1. Clean up Resolved Age
# General
enpro_dat$RESOLVED.AGE_Clean <- enpro_dat$RESOLVED.AGE
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="Reading Error"] <- NA
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="No Age"] <- NA
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE==""] <- NA
# Cowichan R Proj Age Errors
enpro_dat$Year <- enpro_dat$YEAR
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="01" &
                                enpro_dat$Year==2007 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "21"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="02" &
                                enpro_dat$Year==2007 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "31"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="03" &
                                enpro_dat$Year==2007 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "41"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="03" &
                                enpro_dat$Year==2007 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment" &
                                enpro_dat$AGE_GR=="1M"] <- "21"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="2" &
                                enpro_dat$Year==2012 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "21"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="3" &
                                enpro_dat$Year==2015 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "31"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="4" &
                                enpro_dat$Year==2015 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "41"
enpro_dat$RESOLVED.AGE_Clean[enpro_dat$RESOLVED.AGE=="5" &
                                enpro_dat$Year==2012 &
                                enpro_dat$PROJECT=="Cowichan River River Assessment"] <- "51"

# 2. Fill gaps in Resolved Age with Gilbert rich Ages from PADs
enpro_dat$AGE_GR_Clean[enpro_dat$AGE_GR==""] <- NA
enpro_dat <- enpro_dat %>%
  mutate(AGE_R_GR = ifelse(is.na(RESOLVED.AGE_Clean), 
                           AGE_GR_Clean, RESOLVED.AGE_Clean))

# 3. Estimate freshwater age
# Clean up ENPRO: Smolt or Yearling (Freshwater age)
enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean <- enpro_dat$ENPRO..SMOLT.OR.YEARLING
enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean[enpro_dat$ENPRO..SMOLT.OR.YEARLING==""] <- NA
enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean[enpro_dat$ENPRO..SMOLT.OR.YEARLING=="Error"] <- NA
enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean[enpro_dat$ENPRO..SMOLT.OR.YEARLING=="#N/A"] <- NA
enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean[enpro_dat$ENPRO..SMOLT.OR.YEARLING=="Missing enpro_data"] <- NA
enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean <- as.numeric(enpro_dat$ENPRO..SMOLT.OR.YEARLING_Clean)

# 4. Estimate Total age
enpro_dat$TOTAL_AGE <- enpro_dat$Year - enpro_dat$BROOD_YEAR

# 5. Combine freshwater and total age to estimate CWT age, clean up result and backfill resolved age with CWT age
enpro_dat <- enpro_dat %>%
  select(TOTAL_AGE, ENPRO..SMOLT.OR.YEARLING_Clean) %>%
  unite("CWT_Age", TOTAL_AGE:ENPRO..SMOLT.OR.YEARLING_Clean, sep="") %>%
  bind_cols(enpro_dat) 

x <- c("3NA", "2NA",
       "4NA", "5NA",
       "8NA", "13NA",
       "NA1", "NANA")

enpro_dat <- enpro_dat %>%
  mutate(CWT_Age = ifelse(CWT_Age %in% x, NA, CWT_Age)) %>%
  mutate(OceanAge = NA) %>%
  mutate(OceanAge = ifelse(is.na(AGE_R_GR), CWT_Age, AGE_R_GR)) %>%
  mutate(OceanAge = ifelse(AGE_R_GR==CWT_Age, AGE_R_GR, CWT_Age)) %>%
  mutate(OceanAge = ifelse(is.na(CWT_Age), AGE_R_GR, CWT_Age))

# 7. Clean up Ocean Age
enpro_dat$OceanAge[enpro_dat$OceanAge=="21"] <- "Ocean-1"
enpro_dat$OceanAge[enpro_dat$OceanAge=="32"] <- "Ocean-1"
enpro_dat$OceanAge[enpro_dat$OceanAge=="43"] <- "Ocean-1"
enpro_dat$OceanAge[enpro_dat$OceanAge=="31"] <- "Ocean-2"
enpro_dat$OceanAge[enpro_dat$OceanAge=="42"] <- "Ocean-2"
enpro_dat$OceanAge[enpro_dat$OceanAge=="53"] <- "Ocean-2"
enpro_dat$OceanAge[enpro_dat$OceanAge=="41"] <- "Ocean-3"
enpro_dat$OceanAge[enpro_dat$OceanAge=="52"] <- "Ocean-3"
enpro_dat$OceanAge[enpro_dat$OceanAge=="63"] <- "Ocean-3"
enpro_dat$OceanAge[enpro_dat$OceanAge=="51"] <- "Ocean-4"
enpro_dat$OceanAge[enpro_dat$OceanAge=="62"] <- "Ocean-4"
enpro_dat$OceanAge[enpro_dat$OceanAge=="61"] <- "Ocean-5"
enpro_dat$OceanAge[enpro_dat$OceanAge=="71"] <- "Ocean-6"

# Correctiosn based on jack lengths
enpro_dat$OceanAge[enpro_dat$Sex_Clean=="Male" &
                        enpro_dat$POHL_Clean<500] <- "Ocean-1"
enpro_dat$OceanAge[enpro_dat$Sex_Clean=="Male" &
                        enpro_dat$OceanAge=="21" &
                        enpro_dat$POHL_Clean>500] <- "Ocean-2"


# Date ####
enpro_dat <- enpro_dat %>% 
  separate(SAMPLE_START_DATE, c("A", "B"),
           sep ="[^[:alnum:]]+") %>%
  mutate(Month = B)


# Sample Source ####
enpro_dat$SampleSource_Clean <- enpro_dat$SAMPLE_SOURCE1


# Database ####
enpro_dat$Database <- "EnPro"


# Choose columns ####
cnbiodata_clean <- enpro_dat %>%
  select(ID, Year, Release_site, Stock_Clean,
        Stock_Clean2,  Sex_Clean, POHL_Clean,
         Month, OceanAge, Database, SampleSite_Clean)

if(out==T) {
  write.csv(enpro_dat, "dat/processed/enpro_clean.csv")  
}




#------------------------------------------------------------------------------#
# KITIMAT R HATCHERY ####
#------------------------------------------------------------------------------#

# Data ####
temp <- read.csv("dat/raw/KitimatR.csv")
temp2 <- read.csv("dat/raw/KitimatR_14_20.csv")

names(temp)
names(temp2)

colnames(temp) <- c("ID", "Year", "Sample_site", "Tag_code",
                    "SEX...FINAL", "Age", "POHL")

colnames(temp2) <- c("ID", "Year", "Sample_site", "Tag_code",
                     "SEX...FINAL", "Age", "POHL")

temp <- temp %>%
  mutate(Age=as.character(Age)) %>%
  mutate(POHL=as.character(POHL))

kit_dat <- temp2 %>%
  select("ID", "Year", "Sample_site", "Tag_code",
         "SEX...FINAL", "Age", "POHL") %>%
  bind_rows(temp)

# Postorbital-hypural length ####
kit_dat$POHL_Clean <- as.numeric(kit_dat$POHL)

# Sex ####
kit_dat$Sex_Clean[kit_dat$SEX...FINAL=="M"] <- "Male"
kit_dat$Sex_Clean[kit_dat$SEX...FINAL=="F"] <- "Female"
kit_dat$Sex_Clean[kit_dat$SEX...FINAL=="J"] <- "Male"

# Ocean Age ####
kit_dat$OceanAge[kit_dat$Age=="2"] <- "Ocean-1"
kit_dat$OceanAge[kit_dat$Age=="21"] <- "Ocean-1"
kit_dat$OceanAge[kit_dat$Age=="32"] <- "Ocean-1"
kit_dat$OceanAge[kit_dat$Age=="3"] <- "Ocean-2"
kit_dat$OceanAge[kit_dat$Age=="31"] <- "Ocean-2"
kit_dat$OceanAge[kit_dat$Age=="42"] <- "Ocean-2"
kit_dat$OceanAge[kit_dat$Age=="4"] <- "Ocean-3"
kit_dat$OceanAge[kit_dat$Age=="41"] <- "Ocean-3"
kit_dat$OceanAge[kit_dat$Age=="52"] <- "Ocean-3"
kit_dat$OceanAge[kit_dat$Age=="5"] <- "Ocean-4"
kit_dat$OceanAge[kit_dat$Age=="51"] <- "Ocean-4"
kit_dat$OceanAge[kit_dat$Age=="62"] <- "Ocean-4"
kit_dat$OceanAge[kit_dat$Age=="6"] <- "Ocean-5"
kit_dat$OceanAge[kit_dat$Age=="7"] <- "Ocean-6"


kit_dat$OceanAge[kit_dat$Sex_Clean=="Male" &
                      kit_dat$POHL_Clean<500] <- "Ocean-1"
kit_dat$OceanAge[kit_dat$Sex_Clean=="Male" &
                      kit_dat$OceanAge=="21" &
                      kit_dat$POHL_Clean>500] <- "Ocean-2"

# Life stage ####
kit_dat$LifeStage_Clean[kit_dat$SEX...FINAL == "J"] <- "Jack"
kit_dat$LifeStage_Clean[kit_dat$OceanAge=="Ocean-1" & kit_dat$Sex_Clean=="Male"] <- "Jack"
kit_dat$LifeStage_Clean[kit_dat$SEX...FINAL == "F"] <- "Adult"
kit_dat$LifeStage_Clean[kit_dat$SEX...FINAL == "M"] <- "Adult"

# Stock ####
kit_dat$Release_site <- "Kitimat R"
kit_dat$Stock_Clean <- kit_dat$Release_site
kit_dat$Stock_Clean2 <- kit_dat$Release_site

# Database ####
kit_dat$Database <- "Kitimat R Hatchery"

temp <- kit_dat %>%
  select(ID, Year, Release_site, Stock_Clean, Stock_Clean2,
         Sex_Clean, POHL_Clean, OceanAge, Database)

if(out==T) {
  write.csv(kit_dat, "dat/processed/kit_clean.csv")
}

cnbiodata_clean <- cnbiodata_clean %>%
  mutate(ID = as.character(ID)) %>%
  add_row(temp)




#------------------------------------------------------------------------------#
# NECHAKO RIVER - DFO ####
#------------------------------------------------------------------------------#

ndfo_dat <- read.csv("dat/raw/nechako_dfo.csv")

# Day, Month & Year ####

# A < 1000
# A = Month
# B = Day
# C = Year

# A > 1000
# A = Year
# B = Month
# C = Year

ndfo_dat <- ndfo_dat %>%
  mutate(date_2 = date) %>%
  separate(date_2, c("A", "B", "C"),
           sep ="[^[:alnum:]]+") %>%
  mutate(Month = ifelse(A < 1000, A, B)) %>%
  mutate(Day = ifelse(A < 1000, B, C)) %>%
  mutate(Year = ifelse(A < 1000, C, A))

# Postorbital-hypural length ####
ndfo_dat <- ndfo_dat %>%
  mutate(POHL_Clean = POHL) %>%
  mutate(POHL_Clean = as.numeric(ifelse(POHL_Clean == "n/a", NA, POHL_Clean)))

# Sex ####
ndfo_dat$Sex_Clean <- ndfo_dat$sex
ndfo_dat$Sex_Clean[ndfo_dat$sex=="f"] <- "Female"
ndfo_dat$Sex_Clean[ndfo_dat$sex=="m"] <- "Male"
ndfo_dat$Sex_Clean[ndfo_dat$sex=="F"] <- "Female"
ndfo_dat$Sex_Clean[ndfo_dat$sex=="M"] <- "Male"
ndfo_dat$Sex_Clean[ndfo_dat$sex==""] <- NA

# Age ####
ndfo_dat$OceanAge[ndfo_dat$G.R=="32"] <- "Ocean-1"
ndfo_dat$OceanAge[ndfo_dat$G.R=="43"] <- "Ocean-1"
ndfo_dat$OceanAge[ndfo_dat$G.R=="31"] <- "Ocean-2"
ndfo_dat$OceanAge[ndfo_dat$G.R=="42"] <- "Ocean-2"
ndfo_dat$OceanAge[ndfo_dat$G.R=="53"] <- "Ocean-2"
ndfo_dat$OceanAge[ndfo_dat$G.R=="41"] <- "Ocean-3"
ndfo_dat$OceanAge[ndfo_dat$G.R=="52"] <- "Ocean-3"
ndfo_dat$OceanAge[ndfo_dat$G.R=="63"] <- "Ocean-3"
ndfo_dat$OceanAge[ndfo_dat$G.R=="51"] <- "Ocean-4"
ndfo_dat$OceanAge[ndfo_dat$G.R=="62"] <- "Ocean-4"
ndfo_dat$OceanAge[ndfo_dat$G.R=="73"] <- "Ocean-4"
ndfo_dat$OceanAge[ndfo_dat$G.R=="61"] <- "Ocean-5"
ndfo_dat$OceanAge[ndfo_dat$G.R=="72"] <- "Ocean-5"

ndfo_dat$OceanAge[ndfo_dat$POHL_Clean<500 & ndfo_dat$Sex_Clean=="Male"] <- "Ocean-1"

ndfo_dat <- ndfo_dat %>%
  mutate(OceanAge = ifelse(OceanAge=="Ocean-1",
                           ifelse(Sex_Clean=="Male",
                                  ifelse(POHL_Clean>500, "Ocean-2", "Ocean-1"),
                                  OceanAge), OceanAge))

# Stock ####
ndfo_dat$SampleSite_Clean <- ndfo_dat$Stock
ndfo_dat$Stock_Clean <- ndfo_dat$Stock
ndfo_dat$Stock_Clean2 <- ndfo_dat$Stock

# Database ####
ndfo_dat$Database <- "Nechako R (DFO)"

if(out==T) {
  write.csv(ndfo_dat, "dat/processed/ndfo_clean.csv")
}

# Join with rest of biodata
temp <- ndfo_dat %>%
  mutate(Year = as.numeric(Year)) %>%
  select(ID, Year, SampleSite_Clean, Stock_Clean, Stock_Clean2, 
         Sex_Clean, POHL_Clean, OceanAge, Database)

cnbiodata_clean <- cnbiodata_clean %>%
  mutate(ID = as.character(ID)) %>%
  add_row(temp)




#------------------------------------------------------------------------------#
# ROBERTSON CR HISTORICAL DATA ####
#------------------------------------------------------------------------------#

rh_dat <- read.csv("dat/raw/robertson_compiled.csv")

# Postorbital-hypural length ####
rh_dat <- rh_dat %>%
  filter(!LENGTH.C.9==0) %>%
  mutate(LENGTH.C.9 = ifelse(LENGTH.C.9=="N/A", NA, LENGTH.C.9)) %>%
  mutate(Length_Clean = as.integer(LENGTH.C.9)) %>%
  mutate(Length_Clean = ifelse(YEAR %in% c(1990:1994), Length_Clean*10, Length_Clean)) %>%
  mutate(POHL_Clean = ifelse(LM.C.9 %in% c("1", "POH"), Length_Clean, NA)) 

nrow(rh_dat)
# [1] 22647

# Sex ####
# Based on sex codes
rh_dat$Sex_Clean[rh_dat$SEX.C.9==2] <- "Female"
rh_dat$Sex_Clean[rh_dat$SEX.C.9==1] <- "Male"
rh_dat$Sex_Clean[rh_dat$SEX.C.9==3] <- "Male"
rh_dat$Sex_Clean[rh_dat$SEX.C.9=="M"] <- "Male"
rh_dat$Sex_Clean[rh_dat$SEX.C.9=="F"] <- "Female"
rh_dat$Sex_Clean[rh_dat$SEX.C.9=="J"] <- "Male"


# Ocean Age ####
rh_dat$OceanAge <- NA
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="2"] <- "Ocean-1"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="21"] <- "Ocean-1"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="32"] <- "Ocean-1"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="43"] <- "Ocean-1"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="3"] <- "Ocean-2"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="31"] <- "Ocean-2"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="42"] <- "Ocean-2"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="53"] <- "Ocean-2"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="4"] <- "Ocean-3"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="41"] <- "Ocean-3"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="52"] <- "Ocean-3"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="5"] <- "Ocean-4"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="51"] <- "Ocean-4"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="6"] <- "Ocean-5"
rh_dat$OceanAge[rh_dat$S_AGE.C.9=="61"] <- "Ocean-5"

rh_dat <- rh_dat %>%
  mutate(OceanAge = ifelse(OceanAge=="Ocean-1",
                           ifelse(Sex_Clean=="Male",
                                  ifelse(POHL_Clean>500, "Ocean-2", "Ocean-1"),
                                  OceanAge), OceanAge))

# Fill gaps in ocean age based on CWT age, where no scale age data present.
# Leave out instances where CWT age==1, not interested in fish where total age
# suggests they have not spent time in the ocean.

# Clean up CWT Age
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
temp <- rh_dat %>%
  mutate(CWT_Age_Clean=as.numeric(CWT_AGE.C.9))

unique(temp$CWT_Age)
# Calculate CWT age using brood year entries.
temp2 <- temp %>%
  filter(CWT_Age_Clean>1000)
unique(temp2$CWT_Age_Clean)
temp2$CWT_Age_Clean <- temp2$YEAR - temp2$CWT_Age_Clean
unique(temp2$CWT_Age_Clean)
# Get entries with CWT age and bind rows with calculated CWT age.
temp2 <- temp %>%
  filter(!CWT_Age_Clean>1000) %>%
  bind_rows(temp2)
unique(temp2$CWT_Age_Clean)
# Recombine numerically compatible/incompatible entries to reform full data-frame
temp2 <- temp %>%
  filter(is.na(CWT_Age_Clean)) %>%
  bind_rows(temp2)
unique(temp2$CWT_Age_Clean)
nrow(temp2)
# [1] 22647

# Take rows with numerically incompatible entries from this data-frame. Split entries into constituent parts 
# and select values for CWT age (<80)
temp <- temp2 %>%
  mutate(A = as.numeric(CWT_AGE.C.9)) %>%
  filter(is.na(A)) %>%
  separate(CWT_AGE.C.9, c("B", "C", "D"),
           sep ="[^[:alnum:]]+", remove = F) %>%
  mutate(CWT_Age_Clean = as.numeric(ifelse(C<80, C,
                                           ifelse(D<80, D, NA)))) %>%
  select(-A, -B, -C, -D)
unique(temp$CWT_Age_Clean)
nrow(temp)

# Select numerically compatible entries and recombine with newly cleaned 'CWT_Age_Clean' variable
temp2 <- temp2 %>%
  mutate(A = as.numeric(CWT_AGE.C.9)) %>%
  filter(!is.na(A)) %>%
  select(-A) %>%
  bind_rows(temp)
nrow(temp2)
# [1] 22647
unique(temp2$CWT_Age_Clean)

rh_dat <- temp2

# Check that CWT_age_Clean matches respective values in CWT_AGE.C.9
rh_dat %>%
  select(CWT_Age_Clean, CWT_AGE.C.9, YEAR) %>%
  distinct()

# Select rows with no entries under ocean age and add based on CWT_age_Clean (CWT age - 1).

# Select rows with no entries under ocean age, estimate GR age using 
# CWT_age_Clean and FW age. Where no FW age, assume as 1.
temp <- rh_dat %>%
  filter(is.na(OceanAge)) %>%
  unite("GR_age", CWT_Age_Clean, FW.Age, sep="", remove=F)

temp$OceanAge[temp$GR_age=="2"] <- "Ocean-1"
temp$OceanAge[temp$GR_age=="21"] <- "Ocean-1"
temp$OceanAge[temp$GR_age=="32"] <- "Ocean-1"
temp$OceanAge[temp$GR_age=="43"] <- "Ocean-1"
temp$OceanAge[temp$GR_age=="3"] <- "Ocean-2"
temp$OceanAge[temp$GR_age=="31"] <- "Ocean-2"
temp$OceanAge[temp$GR_age=="42"] <- "Ocean-2"
temp$OceanAge[temp$GR_age=="53"] <- "Ocean-2"
temp$OceanAge[temp$GR_age=="4"] <- "Ocean-3"
temp$OceanAge[temp$GR_age=="41"] <- "Ocean-3"
temp$OceanAge[temp$GR_age=="5"] <- "Ocean-4"
temp$OceanAge[temp$GR_age=="51"] <- "Ocean-4"
temp$OceanAge[temp$GR_age=="6"] <- "Ocean-5"
temp$OceanAge[temp$GR_age=="61"] <- "Ocean-5"

# Select rows with ocean age from scale age and add ocean age from CWT age.
temp2 <- rh_dat %>%
  filter(!is.na(OceanAge)) %>%
  bind_rows(temp)
nrow(temp2)
x <- temp2 %>%
  filter(is.na(OceanAge))
nrow(x)
# Corrections to ocean age based on length
temp2$OceanAge[temp2$POHL_Clean>500 & temp2$OceanAge=="Ocean-1" & temp2$Sex_Clean=="Male"] <- "Ocean-2"
temp2$OceanAge[temp2$POHL_Clean<501 & temp2$Sex_Clean=="Male"] <- "Ocean-1"

# Check corresponding entries of GR_age & S_AGE.C.9 (are there 
# inconsistencies where both are present?)
temp2 %>%
  filter(!is.na(CWT_Age_Clean)) %>%
  select(OceanAge, S_AGE.C.9, GR_age, CWT_Age_Clean, FW.Age) %>%
  distinct()

rh_dat <- temp2

# Life stage ####
# Based on sex codes
unique(rh_dat$SEX.C.9)
# "1", "2", "3", "M", "F", "J", "UNKNWN", "?", "", " ", "u", "UNKNOWN"

rh_dat$LifeStage_Clean[rh_dat$SEX.C.9==1] <- "Adult"
rh_dat$LifeStage_Clean[rh_dat$SEX.C.9==2] <- "Adult"
rh_dat$LifeStage_Clean[rh_dat$SEX.C.9==3] <- "Jack"
rh_dat$LifeStage_Clean[rh_dat$SEX.C.9=="M"] <- "Adult"
rh_dat$LifeStage_Clean[rh_dat$SEX.C.9=="F"] <- "Adult"
rh_dat$LifeStage_Clean[rh_dat$SEX.C.9=="J"] <- "Jack"
# Corrections based on length
rh_dat %>%
  mutate(LifeStage_Clean = ifelse(Sex_Clean=="Male",
                                  ifelse(POHL_Clean<501, "Jack", "Adult"), "Adult"))
rh_dat$LifeStage_Clean[rh_dat$POHL_Clean>500] <- "Adult"

# Check consistency in lengths across age and life stage
rh_dat %>%
  filter(LifeStage_Clean=="Jack") %>%
  select(POHL_Clean) %>%
  distinct() %>%
  arrange(POHL_Clean)

# Area - Final Use ####
rh_dat$FinalUse_Clean <- NA

# Stock ####
rh_dat$SampleSite_Clean <- "Robertson Cr"
rh_dat$Stock_Clean <- "Robertson Cr"
rh_dat$Stock_Clean2 <- "Robertson Cr"

# Database ####
rh_dat$Database <- "SEP Historical"

# Year
rh_dat$Year <- rh_dat$YEAR

# Out
if(out==T) {
  write.csv(rh_dat, "dat/processed/rh_clean.csv")
}

# Select columns
temp <- rh_dat %>%
  select(ID, Year, Stock_Clean, Stock_Clean2, SampleSite_Clean,
         Sex_Clean, POHL_Clean, OceanAge, Database)

cnbiodata_clean <- cnbiodata_clean %>%
  mutate(ID = as.character(ID)) %>%
  add_row(temp)




#------------------------------------------------------------------------------#
# CHILLIWACK R HISTORICAL DATA ####
#------------------------------------------------------------------------------#

ch_dat <- read.csv("dat/raw/chilliwack_compiled.csv")

# Postorbital-hypural length ####
ch_dat <- ch_dat %>%
  mutate(Length_Clean = as.numeric(LENGTH.C.9)) %>%
  filter(!Length_Clean==0) %>%
  mutate(POHL_Clean = ifelse(LM.C.9==1, Length_Clean, NA)) 
# %>%
#   filter(!is.na(POHL_Clean))

ch_dat$Year <- ch_dat$YEAR

ch_dat %>%
  group_by(LM.C.9) %>%
  summarise(n())

# Sex ####
# Based on sex codes
ch_dat$Sex_Clean[ch_dat$SEX.C.9==2] <- "Female"
ch_dat$Sex_Clean[ch_dat$SEX.C.9==1] <- "Male"
ch_dat$Sex_Clean[ch_dat$SEX.C.9==3] <- "Male"

# Ocean age ####
# From scale age
ch_dat$OceanAge <- NA
ch_dat$OceanAge[ch_dat$S_AGE.C.9=="21"] <- "Ocean-1"
ch_dat$OceanAge[ch_dat$S_AGE.C.9=="32"] <- "Ocean-1"
ch_dat$OceanAge[ch_dat$S_AGE.C.9=="31"] <- "Ocean-2"
ch_dat$OceanAge[ch_dat$S_AGE.C.9=="42"] <- "Ocean-2"
ch_dat$OceanAge[ch_dat$S_AGE.C.9=="41"] <- "Ocean-3"
ch_dat$OceanAge[ch_dat$S_AGE.C.9=="51"] <- "Ocean-4"

# Check number of rows with ocean age
n <- ch_dat %>%
  filter(!is.na(OceanAge))
nrow(n)
# 622

# Fill gaps in ocean age with CWT age; Select rows that don't have scale age
# and add ocean age
temp <- ch_dat %>%
  filter(is.na(OceanAge))
temp$OceanAge[temp$CWT_AGE.C.9=="21"] <- "Ocean-1"
temp$OceanAge[temp$CWT_AGE.C.9=="2"] <- "Ocean-1"
temp$OceanAge[temp$CWT_AGE.C.9=="31"] <- "Ocean-2"
temp$OceanAge[temp$CWT_AGE.C.9=="3"] <- "Ocean-2"
temp$OceanAge[temp$CWT_AGE.C.9=="41"] <- "Ocean-3"
temp$OceanAge[temp$CWT_AGE.C.9=="4"] <- "Ocean-3"
temp$OceanAge[temp$CWT_AGE.C.9=="51"] <- "Ocean-4"
temp$OceanAge[temp$CWT_AGE.C.9=="5"] <- "Ocean-4"

# Check number of rows with ocean age from CWT age
n <- temp %>%
  filter(!is.na(OceanAge))
nrow(n)
# 205

# Select rows with ocean age from scale age and recombine with rows lacking
# scale age and back-filled with CWT age
temp <- ch_dat %>%
  filter(!is.na(OceanAge)) %>%
  bind_rows(temp)

# Check number of rows with ocean age
n <- temp %>%
  filter(!is.na(OceanAge))
nrow(n)
# 827

ch_dat <- temp

ch_dat <- ch_dat %>%
  mutate(OceanAge = ifelse(OceanAge=="Ocean-1",
                           ifelse(Sex_Clean=="Male",
                                  ifelse(POHL_Clean>500, "Ocean-2", "Ocean-1"),
                                  OceanAge), OceanAge))

# Stock ####
ch_dat$Stock_Clean <- "Chilliwack R"
ch_dat$Stock_Clean2 <- "Chilliwack R"
ch_dat$SampleSite_Clean <- "Chilliwack R"

# Database ####
ch_dat$Database <- "SEP Historical"

if(out==T) {
  write.csv(ch_dat, "dat/processed/ch_clean.csv")
}

# Select columns
temp <- ch_dat %>%
  select(ID, Year, Stock_Clean, Stock_Clean2, SampleSite_Clean,
         Sex_Clean, POHL_Clean, OceanAge, Database) %>%
  filter(!Year==1990)

cnbiodata_clean <- cnbiodata_clean %>%
  mutate(ID = as.character(ID)) %>%
  add_row(temp)


#------------------------------------------------------------------------------#
# RMIS ####

# Load releases
rls <- read.csv("dat/raw/rmis/rls_50_21.csv")

# Load recovery files
fns <- list.files("dat/raw/rmis")
fns <- fns[str_detect(fns, "rcv")]
fns <- fns[str_detect(fns, "csv")]

# Compile recovery files into one data-frame
rcv <- data.frame()
i=1
for(i in 1:length(fns)) {
  fn <- fns[i]
  temp <- read.csv(paste("dat/raw/rmis/",fn,"", sep=""))

  rcv <- rbind(rcv, temp)
}

# Select Chinook records
rcv2 <- rcv %>%
  filter(species==1) %>%
  filter(!tag_code=="")

unique(rcv2$Year)

rcv2$Month <- substr(rcv2$recovery_date, 5, 6)

rls2 <- rls %>%
  filter(species==1) %>%
  filter(!tag_code_or_release_id=="") %>%
  rename(tag_code = tag_code_or_release_id) %>%
  group_by(tag_code) %>%
  select(tag_code, tag_type, species, run, brood_year,
         last_release_date, release_location_code, release_location_name,
         hatchery_location_code, hatchery_location_name, stock_location_code,
         stock_location_name, release_stage)

rls2 %>%
  group_by(release_stage) %>%
  summarise(n())

rls2$release_year <- substr(rls2$last_release_date, 1, 4)

# Combine rls & rcv

rmis <- rcv2 %>%
  group_by(tag_code) %>%
  select(tag_code, tag_type, species, run_year,
         fishery, gear, sex, weight, weight_code,
         weight_type, length, length_code, length_type) %>%
  left_join(rls2) %>%
  ungroup()

# Postorbital-hypural length ####
rmis$POHL_Clean <- rmis$length
rmis <- rmis %>%
  filter(!is.na(POHL_Clean))

# Sex ####
rmis$Sex_Clean <- NA
rmis$Sex_Clean[rmis$sex=="M"] <- "Male"
rmis$Sex_Clean[rmis$sex=="F"] <- "Female"
rmis$Sex_Clean[rmis$sex==""] <- NA

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
rmis <- rmis %>%
  mutate(cwt_age = as.integer(run_year) - as.integer(brood_year)) %>%
  filter(!is.na(cwt_age)) %>%
  filter(!is.na(fw_age)) %>%
  unite("gr_age", cwt_age, fw_age, sep="", remove = F)

rmis$OceanAge[rmis$gr_age=="21"] <- "Ocean-1"
rmis$OceanAge[rmis$gr_age=="32"] <- "Ocean-1"
rmis$OceanAge[rmis$gr_age=="43"] <- "Ocean-1"
rmis$OceanAge[rmis$gr_age=="31"] <- "Ocean-2"
rmis$OceanAge[rmis$gr_age=="42"] <- "Ocean-2"
rmis$OceanAge[rmis$gr_age=="53"] <- "Ocean-2"
rmis$OceanAge[rmis$gr_age=="41"] <- "Ocean-3"
rmis$OceanAge[rmis$gr_age=="52"] <- "Ocean-3"
rmis$OceanAge[rmis$gr_age=="51"] <- "Ocean-4"
rmis$OceanAge[rmis$gr_age=="62"] <- "Ocean-4"
rmis$OceanAge[rmis$gr_age=="61"] <- "Ocean-5"
rmis$OceanAge[rmis$gr_age=="72"] <- "Ocean-5"
rmis$OceanAge[rmis$gr_age=="71"] <- "Ocean-6"
rmis$OceanAge[rmis$gr_age=="82"] <- "Ocean-6"

# Corrections based on length
rmis$OceanAge[rmis$POHL_Clean<501 & rmis$Sex_Clean=="Male"] <- "Ocean-1"
rmis$OceanAge[rmis$POHL_Clean>500 & rmis$Sex_Clean=="Male" & rmis$OceanAge=="Ocean-1"] <- "Ocean-2"

# Remove Ocean-6 fish due to inconsistencies in length distribution
rmis <- rmis %>%
  filter(!OceanAge=="Ocean-6")

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
rmis$Stock_Clean <- rmis$Release_site
rmis$Stock_Clean2 <- rmis$Release_site



# Database ####
rmis$Database <- "RMIS"

# ID ####
rmis$A <- "RMIS"
rmis$B <- c(1:nrow(rmis))
rmis <- rmis %>%
  unite("ID", A, B, sep="")

# Year ####
rmis$Year <- rmis$run_year

if(out==T) {
  write.csv(rmis, "dat/processed/rmis_clean.csv")
}

temp <- rmis %>%
  select(ID, Year, Stock_Clean, Stock_Clean2, Release_site,
         Sex_Clean, POHL_Clean, OceanAge, Database)

cnbiodata_clean <- cnbiodata_clean %>%
  mutate(ID = as.character(ID)) %>%
  add_row(temp)




#------------------------------------------------------------------------------#
# COMPLETE DATABASE ####
#------------------------------------------------------------------------------#

if(out==T) {
  write.csv(cnbiodata_clean, "dat/processed/cnbiodata_clean.csv")
}


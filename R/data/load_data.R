library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------#
# EnPro ####
#------------------------------------------------------------------------------#
# Data ####
enpro_dat <- read_xlsx("data-raw/enpro/Kent_AllChinook_BiodataDB.xlsx",
                       col_types = rep("text", 121))
erin_dat <- read_xlsx("data-raw/meta/erin_thermalmark.xlsx",
                      col_types = rep("text", 133))

#------------------------------------------------------------------------------#
# KITIMAT R HATCHERY ####
#------------------------------------------------------------------------------#
# Data ####
kit_dat <- read.csv("data-raw/nonenpro/KitimatR.csv")
kit_dat2 <- read.csv("data-raw/nonenpro/KitimatR_14_20.csv")

#------------------------------------------------------------------------------#
# NECHAKO RIVER - DFO ####
#------------------------------------------------------------------------------#
ndfo_dat <- read.csv("data-raw/nonenpro/nechako_dfo.csv")

#------------------------------------------------------------------------------#
# ROBERTSON CR HISTORICAL DATA ####
#------------------------------------------------------------------------------#
rh_dat <- read.csv("data-raw/historical/robertson_compiled.csv")

#------------------------------------------------------------------------------#
# CHILLIWACK R HISTORICAL DATA ####
#------------------------------------------------------------------------------#
ch_dat <- read.csv("data-raw/historical/chilliwack_compiled.csv")

#------------------------------------------------------------------------------#
# RMIS ####
#------------------------------------------------------------------------------#
# Load releases
rls <- read.csv("data-raw/rmis/rls_50_21.csv")

# Load recovery files
my_dir <- "data-raw/rmis/rcv/"
list.files(my_dir)
rcv <- paste0(my_dir, list.files(my_dir)) %>%
  map_df(~read_csv(., col_types = paste(c(rep("c", 43)), collapse = "")))

path <- "data-raw/raw_data.Rdata"

save.image(file = path)

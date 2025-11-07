library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------#
# EnPro ####
#------------------------------------------------------------------------------#
# Data ####
enpro_dat <- read_xlsx("data/kokanee/SEP/SEP_ENPRO_CN.xlsx",
                       col_types = rep("text", 121))
erin_dat <- read_xlsx("data/kokanee/SEP/SEP_OTO_THERM_CN.xlsx",
                      col_types = rep("text", 133))

#------------------------------------------------------------------------------#
# KITIMAT R HATCHERY ####
#------------------------------------------------------------------------------#
# Data ####
kit_dat <- read.csv("data/kokanee/KITIMAT/KitimatR.csv")
kit_dat2 <- read.csv("data/kokanee/KITIMAT/KitimatR_14_20.csv")

#------------------------------------------------------------------------------#
# NECHAKO RIVER - DFO ####
#------------------------------------------------------------------------------#
ndfo_dat <- read.csv("data/kokanee/NECHAKO/nechako_dfo.csv")

#------------------------------------------------------------------------------#
# ROBERTSON CR HISTORICAL DATA ####
#------------------------------------------------------------------------------#
rh_dat <- read.csv("data/kokanee/SEP/SEP_HISTORICAL/SEP_HISTORICAL_CN_ROBERTSON.csv")

#------------------------------------------------------------------------------#
# CHILLIWACK R HISTORICAL DATA ####
#------------------------------------------------------------------------------#
ch_dat <- read.csv("data/kokanee/SEP/SEP_HISTORICAL/SEP_HISTORICAL_CN_CHILLIWACK.csv")

#------------------------------------------------------------------------------#
# RMIS ####
#------------------------------------------------------------------------------#
# Load releases
rls <- read.csv("data/kokanee/RMIS/rls_50_21.csv")

# Load recovery files
my_dir <- "data/kokanee/RMIS/rcv/"
list.files(my_dir)
rcv <- paste0(my_dir, list.files(my_dir)) %>%
  map_df(~read_csv(., col_types = paste(c(rep("c", 43)), collapse = "")))

path <- "data/kokanee/kokanee.Rdata"

save.image(file = path)

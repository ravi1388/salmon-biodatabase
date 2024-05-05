# Notes ####
# Use this script to conduct logistic regressions of sex compositions calculated from 
# EPADS River Returns

# Packages ####
library(tidyverse)
library(data.table)

# Data ####
rm(list=ls())
dat <- read.csv("dat/raw/PSFEscCN2000+_oee.csv")

# Variables ####
t_age <- 2
t_yr <- 4

# CLEANING ####

# Ocean age ####
dat$OceanAge <- NA
dat$OceanAge[dat$Age=="2"] <- "Ocean-1"
dat$OceanAge[dat$Age=="3"] <- "Ocean-2"
dat$OceanAge[dat$Age=="4"] <- "Ocean-3"
dat$OceanAge[dat$Age=="5"] <- "Ocean-4"
dat$OceanAge[dat$Age=="6"] <- "Ocean-5"

# Stock ####
dat$Stock_Clean <- dat$STOCK_NAME
dat$Stock_Clean[dat$STOCK_NAME=="Atnarko R Up"] <- "Atnarko R"
dat$Stock_Clean[dat$STOCK_NAME=="Atnarko R Low"] <- "Atnarko R"
dat$Stock_Clean[dat$STOCK_NAME=="Kitsum Bel Canyon"] <- "Kitsumkalum R"
dat$Stock_Clean[dat$STOCK_NAME=="Kitsum Abv Canyon"] <- "Kitsumkalum R"
dat$Stock_Clean[dat$STOCK_NAME=="Kitimat R Low"] <- "Kitimat R"
dat$Stock_Clean[dat$STOCK_NAME=="Kitimat R Up"] <- "Kitimat R"
dat$Stock_Clean[dat$STOCK_NAME=="Shuswap R Middle"] <- "Shuswap R"
dat$Stock_Clean[dat$STOCK_NAME=="Shuswap R Low"] <- "Shuswap R"


dat <- dat %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  filter(!Observed==0)

dat$Year <- dat$RECOVERY_YEAR

# Filter dataset for minimum number of age classes per year
dat2 <- dat %>%
  select(Stock_Clean, Year, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n=n()) %>%
  filter(n>t_age) %>%
  group_by(Stock_Clean, Year) %>%
  left_join(dat)

# Sum return estimates for each age class, per year and Stock
df <- dat2 %>%
  select(Stock_Clean, Year, OceanAge, Observed, Estimated, Expanded) %>%
  group_by(Stock_Clean, Year, OceanAge) %>%
  summarise_at(c("Observed", "Estimated", "Expanded"), sum)

# Sum return estimates for each Stock, per year
df2 <- dat2 %>%
  select(Stock_Clean, Year, OceanAge, Observed, Estimated, Expanded) %>%
  group_by(Stock_Clean, Year) %>%
  mutate(Observed_sum = Observed) %>%
  mutate(Estimated_sum = Estimated) %>%
  mutate(Expanded_sum = Expanded) %>%
  summarise_at(c("Observed_sum", "Estimated_sum", "Expanded_sum"), sum) %>%
  # Combine Stock estimates per year with corresponding age class estimates
  right_join(df) %>%
  # Calculate age compositions per year and Stock
  mutate(Observed_p = Observed/Observed_sum) %>%
  mutate(Estimated_p = Estimated/Estimated_sum) %>%
  mutate(Expanded_p = Expanded/Expanded_sum) %>%
  unite("stk_yr_age", Stock_Clean, Year, OceanAge, remove = F)

# Gather age comps by return estimate
df3 <- df2 %>%
  ungroup() %>%
  select(Stock_Clean, Year, OceanAge,
         Observed_p, Estimated_p, Expanded_p) %>%
  gather("Indicator", "p_age", Observed_p:Expanded_p) %>%
  mutate(Indicator = ifelse(Indicator=="Observed_p", "Observed",
                            ifelse(Indicator=="Estimated_p", "Estimated", "Expanded")))

df3 <- df2 %>%
  ungroup() %>%
  select(Stock_Clean, Year, OceanAge,
         Observed, Estimated, Expanded) %>%
  gather("Indicator", "Counts_age", Observed:Expanded) %>%
  right_join(df3)

df3 <- df2 %>%
  ungroup() %>%
  select(Stock_Clean, Year, OceanAge,
         Observed_sum, Estimated_sum, Expanded_sum) %>%
  gather("Indicator", "Counts_sum", Observed_sum:Expanded_sum) %>%
  mutate(Indicator = ifelse(Indicator=="Observed_sum", "Observed",
                            ifelse(Indicator=="Estimated_sum", "Estimated", "Expanded"))) %>%
  right_join(df3)

# Filter out Stocks with total number of years below threshold
df4 <- df3 %>%
  select(Stock_Clean, Indicator, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, Indicator) %>%
  summarise(n=n()) %>%
  filter(n>t_yr) %>%
  group_by(Stock_Clean, Indicator) %>%
  right_join(df3) %>%
  filter(!is.na(n))

df4 %>%
  filter(Indicator=="Observed") %>%
  ggplot(aes(x=Year, y=p_age, fill=OceanAge)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Stock_Clean) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1")

write.csv(df4, "dat/processed/oee.csv")




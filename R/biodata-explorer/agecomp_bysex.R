# Notes ####
# Use this script to calculate age compositions from EPADS Obs-Est-Exp Returns


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

# Sex ####
dat <- dat %>%
  mutate(Sex_Clean = ifelse(SEX_MAT_NAME=="Males", "Male",
                            ifelse(SEX_MAT_NAME=="Females", "Female", NA)))

dat <- dat %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  filter(Sex_Clean %in% c("Male", "Female")) %>%
  filter(!Observed==0)

dat$Year <- dat$RECOVERY_YEAR

# Filter dataset for minimum number of age classes per year
dat2 <- dat %>%
  select(Stock_Clean, Year, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n_age=n()) %>%
  filter(n_age>t_age) %>%
  group_by(Stock_Clean, Year) %>%
  left_join(dat)

# Filter dataset for minimum number of sex classes per year
dat2 <- dat2 %>%
  select(Stock_Clean, Year, Sex_Clean) %>%
  distinct() %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n_sex=n()) %>%
  filter(n_sex>1) %>%
  group_by(Stock_Clean, Year) %>%
  left_join(dat2)
  
# Sum return estimates for each age class, per year and Stock
df <- dat2 %>%
  select(Stock_Clean, Year, OceanAge, Sex_Clean, Observed, Estimated, Expanded) %>%
  group_by(Stock_Clean, Year, OceanAge, Sex_Clean) %>%
  summarise_at(c("Observed", "Estimated", "Expanded"), sum)

# Sum return estimates for each Stock, per year
df2 <- dat2 %>%
  select(Stock_Clean, Year, OceanAge, Sex_Clean, Observed, Estimated, Expanded) %>%
  group_by(Stock_Clean, Sex_Clean, Year) %>%
  mutate(Observed_sum = Observed) %>%
  mutate(Estimated_sum = Estimated) %>%
  mutate(Expanded_sum = Expanded) %>%
  summarise_at(c("Observed_sum", "Estimated_sum", "Expanded_sum"), sum) %>%
  # Combine Stock estimates per year with corresponding age class estimates
  right_join(df) %>%
  # Calculate age compositions per year and Stock
  mutate(Observed_p = Observed/Observed_sum) %>%
  mutate(Estimated_p = Estimated/Estimated_sum) %>%
  mutate(Expanded_p = Expanded/Expanded_sum)

# Gather age comps by return estimate
df3 <- df2 %>%
  ungroup() %>%
  select(Stock_Clean, Year, OceanAge, Sex_Clean,
         Observed_p, Estimated_p, Expanded_p) %>%
  gather("Indicator", "p_age", Observed_p:Expanded_p) %>%
  mutate(Indicator = ifelse(Indicator=="Observed_p", "Observed",
                            ifelse(Indicator=="Estimated_p", "Estimated", "Expanded")))

df3 <- df2 %>%
  ungroup() %>%
  select(Stock_Clean, Year, OceanAge, Sex_Clean,
         Observed, Estimated, Expanded) %>%
  gather("Indicator", "Counts_age", Observed:Expanded) %>%
  right_join(df3)

df3 <- df2 %>%
  ungroup() %>%
  select(Stock_Clean, Year, OceanAge, Sex_Clean,
         Observed_sum, Estimated_sum, Expanded_sum) %>%
  gather("Indicator", "Counts_sum", Observed_sum:Expanded_sum) %>%
  mutate(Indicator = ifelse(Indicator=="Observed_sum", "Observed",
                            ifelse(Indicator=="Estimated_sum", "Estimated", "Expanded"))) %>%
  right_join(df3)

# Filter out Stocks with total number of years below threshold
df4 <- df3 %>%
  select(Stock_Clean, Sex_Clean, Indicator, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, Sex_Clean, Indicator) %>%
  summarise(n=n()) %>%
  filter(n>t_yr) %>%
  group_by(Stock_Clean, Sex_Clean, Indicator) %>%
  right_join(df3) %>%
  filter(!is.na(n))

df4 %>%
  filter(Indicator=="Observed") %>%
  ggplot(aes(x=Year, y=p_age, fill=OceanAge)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Stock_Clean~Sex_Clean) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1")

ggsave("out/trends/agecomp/bysex_epad.jpg", width = 15, height = 8)

write.csv(df4, "out/trends/agecomp/oee_bysex.csv")




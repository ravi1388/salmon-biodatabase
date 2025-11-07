# Notes ####
# Use this script to calculate sex compositions from EPADS River Returns

# Packages ####
library(tidyverse)
library(lemon)
library(data.table)

# Data ####
rm(list=ls())
dat <- read.csv("dat/raw/PSFEscCN2000+_rr.csv")

# Variables ####
t_yr <- 4
t_sex <- 1


# Sex ####
dat$Sex_Clean <- dat$SEX_MAT_NAME
dat$Sex_Clean[dat$Sex_Clean=="Jack"] <- "Male"
dat$Sex_Clean[dat$Sex_Clean=="Jill"] <- "Female"

# Stock ####
dat$Stock_Clean <- dat$RETURN_SITE_NAME
dat$Stock_Clean[dat$Stock_Clean=="Atnarko R Up"] <- "Atnarko R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Atnarko R Low"] <- "Atnarko R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Atnarko R Mid"] <- "Atnarko R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Atnarko R Up"] <- "Atnarko R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Clearwater Low"] <- "Clearwater R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Clearwater Up"] <- "Clearwater R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Kitsum Bel Canyon"] <- "Kitsumkalum R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Kitsum Abv Canyon"] <- "Kitsumkalum R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Kitimat R Low"] <- "Kitimat R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Kitimat R Up"] <- "Kitimat R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Shuswap R Middle"] <- "Shuswap R"
dat$Stock_Clean[dat$RETURN_SITE_NAME=="Shuswap R Low"] <- "Shuswap R"

dat <- dat %>%
  filter(Sex_Clean %in% c("Male", "Female")) 

df <- dat %>%
  select(Stock_Clean, RECOVERY_YEAR, Sex_Clean,
         Total:X24Dead.Pitch...Usable.Count.Total.River) %>%
  group_by(Stock_Clean, RECOVERY_YEAR, Sex_Clean) 

df[is.na(df)] <- 0

df2 <- df %>%
  mutate(Enumerated = rowSums(across(Total:X24Dead.Pitch...Usable.Count.Total.River))) %>%
  select(Stock_Clean, RECOVERY_YEAR, 
         Sex_Clean, Enumerated) %>%
  unite("stk_yr", Stock_Clean, RECOVERY_YEAR, remove = F) %>%
  filter(!Enumerated==0)%>%
  arrange(Sex_Clean, Stock_Clean, RECOVERY_YEAR)

df3 <- df2 %>%
  group_by(Stock_Clean, RECOVERY_YEAR) %>%
  summarise(Enumerated_sum = sum(Enumerated))

df4 <- df2 %>%
  group_by(Stock_Clean, RECOVERY_YEAR, Sex_Clean) %>%
  summarise(Enumerated_sex = sum(Enumerated)) %>%
  right_join(df3) %>%
  mutate(p = Enumerated_sex/Enumerated_sum) %>%
  mutate(Indicator = "EPAD - River returns") %>%
  select(Stock_Clean, RECOVERY_YEAR,
         p, Sex_Clean, Indicator) %>%
  filter(!p==1)

df5 <- df4 %>%
  select(Stock_Clean, RECOVERY_YEAR) %>%
  distinct() %>%
  group_by(Stock_Clean) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr > t_yr) %>%
  left_join(df4) %>%
  mutate(Year = RECOVERY_YEAR) %>%
  select(-RECOVERY_YEAR)

df5 %>%
  ggplot(aes(x=Year, y=p, fill=Sex_Clean)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Stock_Clean) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  labs(x="Year", y="Sex composition", fill="Sex")

write.csv(df5, "dat/processed/comps_rr.csv")

rm(list=ls())

library(tidyverse)
library(lemon)

# Data
cnbiodata_clean <- read.csv("dat/processed/cnbiodata_clean.csv")

# Variables
t_pt <- 0
t_yr <- 4



# POHL
dat <- cnbiodata_clean %>%
  select(Stock_Clean, Year, POHL_Clean) %>%
  filter(!is.na(POHL_Clean)) %>%
  filter(!is.na(Stock_Clean)) %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n()) %>%
  filter(`n()`>t_pt) %>%
  rename(n = `n()`)

dat2 <- dat %>%
  group_by(Stock_Clean) %>%
  summarise(n()) %>%
  filter(`n()`>t_yr) %>%
  select(Stock_Clean) %>%
  left_join(dat) %>%
  mutate(log_n = log(n)) %>%
  mutate(Variable="POHL") %>%
  mutate(Value="POHL")
  
dat2 %>%  
  ggplot(aes(x=Year, y=n)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  facet_wrap(~Stock_Clean)

ggsave("out/inventory/poh.jpg", height = 8, width = 10)

# By Age
dat <- cnbiodata_clean %>%
  select(Stock_Clean, Year, POHL_Clean, OceanAge) %>%
  filter(!is.na(POHL_Clean)) %>%
  filter(!is.na(Stock_Clean)) %>%
  filter(OceanAge %in% c("Ocean-1",
                         "Ocean-2",
                         "Ocean-3",
                         "Ocean-4",
                         "Ocean-5")) %>%
  group_by(Stock_Clean, OceanAge, Year) %>%
  summarise(n()) %>%
  rename(n = `n()`)

dat <- dat %>%
  group_by(Stock_Clean, Year, OceanAge) %>%
  summarise(sum(n)) %>%
  rename(n_yr = `sum(n)`) %>%
  filter(n_yr>t_pt) %>%
  left_join(dat)

dat2 <- dat %>%
  select(Stock_Clean, Year, OceanAge)%>%
  distinct() %>%
  ungroup() %>%
  select(Stock_Clean, OceanAge) %>%
  group_by(Stock_Clean, OceanAge) %>%
  summarise(n()) %>%
  filter(`n()`>t_yr) %>%
  select(Stock_Clean, OceanAge) %>%
  left_join(dat) %>%
  mutate(log_n = log(n)) %>%
  select(-n_yr) %>%
  mutate(Variable="Ocean age") %>%
  rename(Value = OceanAge) %>%
  bind_rows(dat2)

dat2 %>%
  filter(Variable=="Ocean age") %>%
  ggplot(aes(x=Year, y=n, fill=Value)) +
  geom_bar(stat = "identity", position="stack") +
  theme_bw() +
  facet_wrap(~Stock_Clean) +
  scale_fill_brewer(palette = "Dark2")

ggsave("out/inventory/age.jpg", height = 8, width = 10)

# By Sex
# Number of records for each sex, per Stock and year
dat <- cnbiodata_clean %>%
  select(Stock_Clean, Year, POHL_Clean, Sex_Clean) %>%
  filter(!is.na(POHL_Clean)) %>%
  filter(!is.na(Stock_Clean)) %>%
  filter(Sex_Clean %in% c("Male",
                         "Female")) %>%
  group_by(Stock_Clean, Sex_Clean, Year) %>%
  summarise(n()) %>%
  rename(n = `n()`)

# Filter years below threshold of records per year
dat <- dat %>%
  group_by(Stock_Clean, Year, Sex_Clean) %>%
  summarise(sum(n)) %>%
  rename(n_yr = `sum(n)`) %>%
  filter(n_yr>t_pt) %>%
  left_join(dat)

# Filter Stocks below threshold of years per Stock
dat2 <- dat %>%
  filter(!is.na(Stock_Clean)) %>%
  select(Stock_Clean, Year, Sex_Clean) %>%
  distinct() %>%
  ungroup() %>%
  select(Stock_Clean, Sex_Clean) %>%
  group_by(Stock_Clean, Sex_Clean) %>%
  summarise(n()) %>%
  filter(`n()`>t_yr) %>%
  select(Stock_Clean, Sex_Clean) %>%
  left_join(dat) %>%
  mutate(log_n = log(n))  %>%
  select(-n_yr) %>%
  mutate(Variable="Sex") %>%
  rename(Value = Sex_Clean) %>%
  # Add to dataframe with age and POHL inventory (for use in facet grid below)
  bind_rows(dat2)

# Choose the sex variable and plot all Stocks
dat2 %>%
  filter(Variable=="Sex") %>%
  ggplot(aes(x=Year, y=n, fill=Value)) +
  geom_bar(stat = "identity", position="stack") +
  theme_bw() +
  facet_wrap(~Stock_Clean) +
  scale_fill_brewer(palette = "Dark2")

ggsave("out/inventory/sex.jpg", height = 8, width = 10)

# Facet grid ####
# Sort Stocks by number of samples
stk <- dat2 %>%
  filter(Variable=="POHL") %>%
  select(Stock_Clean, n) %>%
  group_by(Stock_Clean) %>%
  summarise(sum(n)) %>%
  arrange(-`sum(n)`) %>%
  pull(Stock_Clean)

i=1
for(i in 1:7) {
  
  # Set index for filter
  a <- ifelse(i==1, 1, 5*(i-1)+1)
  b <- ifelse(i==7, 32, i*5)
  a
  b
  x <-c(stk[a:b])
  
  # Set order of variables for facet grid
  dat2 %>%
    mutate(Variable_f = factor(Variable, levels = c("POHL",
                                                    "Sex",
                                                    "Ocean age"))) %>%
    # Set order of Stocks based on sample size
    mutate(Stock_Clean_f = factor(Stock_Clean, levels = stk)) %>%
    # Filter Stocks to plot
    filter(Stock_Clean %in% x) %>%
    ggplot(aes(x=Year, y=n, fill=Value)) +
    geom_bar(stat = "identity", position="stack") +
    theme_bw() +
    facet_rep_grid(Stock_Clean_f~Variable_f) +
    scale_fill_brewer(palette = "Dark2") +
    labs(y="Number of records")
  
  ggsave(paste("out/inventory/inventory_",i,".jpg", sep=""), width=8, height=6)
  
}

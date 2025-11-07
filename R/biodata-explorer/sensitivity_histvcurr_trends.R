library(tidyverse)

rm(list=ls())

dat <- read.csv("dat/processed/cnbiodata_clean.csv")

# Variables
t_pt <- 19
t_yr <- 4
t_age <- 2


# Filtering ####
dat <- dat %>%
  filter(Database %in% c("EnPro", "SEP Historical")) %>%
  filter(Stock_Clean %in% c("Robertson Cr", "Chilliwack R")) %>%
  filter(!OceanAge=="Ocean-1" | !Sex_Clean=="Male") %>%
  filter(!is.na(Stock_Clean)) %>%
  filter(!is.na(POHL_Clean)) %>%
  select(Year, POHL_Clean, Database, Stock_Clean)

df2 <- dat %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n_pt=n()) %>%
  filter(n_pt > t_pt) %>%
  group_by(Stock_Clean, Year) %>%
  right_join(dat) %>%
  filter(!is.na(n_pt))

df3 <- df2 %>%
  select(Stock_Clean, Year) %>%
  distinct() %>%
  group_by(Stock_Clean) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr > t_yr) %>%
  group_by(Stock_Clean) %>%
  right_join(df2) %>%
  filter(!is.na(n_yr)) %>%
  group_by(Stock_Clean, Year) %>%
  mutate(POHL_Mean = mean(POHL_Clean))


df3 %>% ggplot(aes(x=Year, y=POHL_Mean)) +
  geom_point(aes(x=Year, y=POHL_Clean, color=Database),
             size=0.5, alpha=0.5, shape=1) +
  geom_point(color="Black") +
  geom_line(stat = "smooth", method = "lm", size = 1, alpha = 0.5, color="Black") +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(y="POHL (mm)") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(Database~Stock_Clean)

ggsave("out/sensitivity/database/sizemean_histvcurr.jpg", width = 9, height = 5)

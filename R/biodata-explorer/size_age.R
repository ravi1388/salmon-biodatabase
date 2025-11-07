# Notes ####
# Use this script to conduct regressions on time series of size at age 
# and produce summary statistics and plots

# Packages ####
library(tidyverse)
library(lemon)
library(tidytext)

# Data ####
rm(list=ls())
# dat <- read.csv("dat/processed/cnbiodata_clean.csv")
dat <- read.csv("dat/processed/kit_clean.csv")
rel <- read.csv("dat/raw/sepreleases.csv")


# Variables ####
t_pt <- 9
t_yr <- 4
t_age <- 1


# Filtering ####
dat <- dat %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  filter(!is.na(Stock_Clean)) %>%
  filter(!is.na(POHL_Clean)) %>%
  select(Year, POHL_Clean, OceanAge, Stock_Clean)

df <- dat %>%
  group_by(Stock_Clean, OceanAge, Year) %>%
  summarise(n_pt=n()) %>%
  filter(n_pt > t_pt) %>%
  group_by(Stock_Clean, OceanAge, Year) %>%
  right_join(dat) %>%
  filter(!is.na(n_pt))

df2 <- df %>%
  select(Stock_Clean, Year, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n_age=n()) %>%
  filter(n_age > t_age)  %>%
  group_by(Stock_Clean, Year) %>%
  right_join(df) %>%
  filter(!is.na(n_age))

df3 <- df2 %>%
  select(Stock_Clean, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, OceanAge) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr > t_yr) %>%
  group_by(Stock_Clean, OceanAge) %>%
  right_join(df) %>%
  filter(!is.na(n_yr))

df3 <- df3 %>%
  select(Stock_Clean, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, OceanAge) %>%
  summarise(n_yr=n()) %>%
  right_join(df3)

df3 %>%
  ggplot(aes(x=Year, y=POHL_Clean, color=OceanAge)) +
  geom_point(alpha=0.1, 
             size=0.5) +
  geom_line(stat = "smooth", method = "lm", size = 1, alpha = 0.5) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(y="POHL (mm)", color = "Ocean age") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Stock_Clean)

write.csv(df3, "out/trends/sizeage/timeseries.csv")
ggsave("out/trends/sizeage/timeseries.jpg", width = 12, height = 8)




# Regressions and rate of change - All ####
age <- unique(df3$OceanAge)
sys <- unique(df3$Stock_Clean)

params <- data.frame()
i=j=1
for(i in 1:length(sys)) {
  for(j in 1:length(age)) {
    sys.choice <- sys[i]
    age.choice <- age[j]
    
    temp <- df3 %>% 
      filter(Stock_Clean==sys[i] & OceanAge==age[j])
    if(nrow(temp)>0) {
      fit <- lm(POHL_Clean ~ Year, dat=temp)
      
      conf <- confint(fit, "Year", level=0.95)
      up <- c(summary(fit)$coefficients[2,1] + conf[1,2])
      low <- c(summary(fit)$coefficients[2,1] - conf[1,2])
      
      x <- data.frame(Stock_Clean=sys.choice,
                      Age=age.choice,
                      Slope=summary(fit)$coefficients[2,1],
                      upr=up,
                      lwr=low,
                      SE=summary(fit)$coefficients[2,2],
                      pval=summary(fit)$coefficients[2,4],
                      Sig=ifelse(summary(fit)$coefficients[2,4]<0.05,"Y","N"),
                      Sig_al=ifelse(summary(fit)$coefficients[2,4]<0.05,1,0.5),
                      Sig_sz=ifelse(summary(fit)$coefficients[2,4]<0.05,4,2),
                      Sig_sp=ifelse(summary(fit)$coefficients[2,4]<0.05,19,1),
                      Sex_sp=19,
                      Rsq=summary(fit)$r.squared,
                      N=unique(temp$n_yr))
      params <- rbind.data.frame(params, x)
    }
  }
}


params <- params %>%
  mutate(Rsq_dec = round(Rsq, 3)) %>%
  mutate(Rsq_sz = (Rsq_dec)*2) %>%
  arrange(Age, pval) %>%
  mutate(R_cl = ifelse(N<(max(params$N)*0.33), "Short","Medium"),) %>%
  mutate(R_cl = ifelse(N>(max(params$N)*0.66), "Long", R_cl)) %>%
  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long")) %>%
  mutate(Sex = "Both") %>%
  mutate(Shp=case_when(Sig=="Y" & 
                         !Stock_Clean %in% c("Stuart R","Nechako R","Harrison R") ~ "Enhanced (<0.05)",
                       Sig=="N" &
                         !Stock_Clean %in% c("Stuart R","Nechako R","Harrison R") ~ "Enhanced",
                       Sig=="Y" & 
                         Stock_Clean %in% c("Stuart R","Nechako R","Harrison R") ~ "Wild/Low enhancement (<0.05)",
                       Sig=="N" & 
                         Stock_Clean %in% c("Stuart R","Nechako R","Harrison R") ~ "Wild/Low enhancement"))

write.csv(params, "out/trends/sizeage/stats.csv")

# Add region to results

params2 <- rel %>%
  mutate(STOCK_NAME = case_when(STOCK_NAME=="Shuswap R Low" ~ "Shuswap R",
                                TRUE ~ STOCK_NAME)) %>%
  mutate(Region = case_when(STOCK_PROD_AREA_CODE %in% c("NCST", "QCI", "SKNA", "NASS") ~ "NC/CC",
                            STOCK_PROD_AREA_CODE %in% c("GSMN", "GSVI", "JNST", "LWFR", "TOMM", "TOMF", "GSMS", "UPFR") ~ "ISC",
                            STOCK_PROD_AREA_CODE %in% c("SWVI", "NWVI") ~ "WCVI",
                            STOCK_PROD_AREA_CODE %in% c("CCST", "RIVR") ~ "NC/CC")) %>%
  select(STOCK_NAME, Region) %>%
  rename("Stock_Clean"=STOCK_NAME) %>%
  distinct() %>%
  group_by(Stock_Clean) %>%
  right_join(params) %>%
  ungroup() %>%
  mutate(Sig_sz = case_when(Sig_sz==2 ~ 3,
                            TRUE ~ 4))

# Scatter plots ####

params2 %>%
  mutate(Stock_Clean = reorder_within(Stock_Clean, -Slope, Sex)) %>%
  ggplot(aes(y=Slope, x=Stock_Clean, color=Age)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_point(size=2,
             aes(shape=Shp)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(breaks=seq(-10,5,5)) +
  labs(y = "Change in POHL, mm.yr-1",
       color = "Ocean age",
       x="Stock") +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
  scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
  scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA"))

ggsave("out/trends/sizeage/rates.jpg", width = 6, height = 5)


# Boxplot ####

params2 %>%
  mutate(Region = fct_relevel(Region, "WCVI", "ISC", "NC/CC")) %>%
  ggplot(aes(y=Slope, x=Region, color=Age)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_boxplot() +
  geom_point(size=2,
             alpha=0.75,
             aes(shape=Shp,
                 fill=Age),
             position=position_jitterdodge(jitter.width = 0.1,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  facet_wrap(~Age,
             ncol=1) +
  scale_y_continuous(breaks=seq(-10,5,5)) +
  labs(y = "Change in POHL, mm.yr-1",
       color = "Age",
       shape = "") +
  guides(fill="none") +
  scale_color_manual(values = c("Ocean-4"="#F4511E",
                                "Ocean-3"="#7CB342",
                                "Ocean-2"="#26C6DA")) +
  scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
  theme(legend.position = "bottom", 
        legend.box="vertical",
        legend.margin=margin())

ggsave("out/trends/sizeage/rates_reg.jpg", width = 6, height = 5)

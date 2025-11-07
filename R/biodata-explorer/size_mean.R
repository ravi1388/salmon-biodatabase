# Notes ####
# Use this script to estimate mean size from biodata, conduct regressions on
# the resulting time series and produce summary statistics and plots

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
t_age <- 2


# Filtering ####
dat <- dat %>%
  filter(!Stock_Clean %in% c("Discovery Pass", "Phillips Lk")) %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  filter(!is.na(Stock_Clean)) %>%
  filter(!is.na(POHL_Clean)) %>%
  select(Year, POHL_Clean, OceanAge, Stock_Clean)

dat %>% 
  group_by(Stock_Clean, Year, OceanAge) %>% 
  summarise(n_pt=n()) %>% 
  ungroup() %>% filter(n_pt>3)

dat %>% 
  distinct(Stock_Clean, Year, OceanAge) %>% 
  group_by(Stock_Clean, OceanAge) %>% 
  summarise(n_yr=n()) %>% 
  ungroup %>% distinct(n_yr)

dat %>% 
  distinct(Stock_Clean, Year, OceanAge) %>% 
  group_by(Stock_Clean, Year) %>% 
  summarise(n_age=n()) %>% filter(n_age==2)
# 2007
  

df <- dat %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n_pt=n()) %>%
  filter(n_pt > t_pt) %>%
  group_by(Stock_Clean, Year) %>%
  right_join(dat) %>%
  filter(!is.na(n_pt))

df2 <- df %>%
  select(Stock_Clean, Year, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean, Year) %>%
  summarise(n_age=n()) %>%
  filter(n_age>t_age) %>%
  group_by(Stock_Clean, Year) %>%
  right_join(df) %>%
  filter(!is.na(n_age))

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

sys <- unique(df3$Stock_Clean)
temp2 <- data.frame()
i=1
for(i in 1:length(sys)) {
  
  temp <- filter(df3, Stock_Clean==sys[i])
  temp <- temp %>%
    mutate(ts_l = max(temp$Year)-min(temp$Year))
  temp2 <- rbind.data.frame(temp2, temp)
  
}

temp2 <- temp2  %>%
  ungroup() %>%
  mutate(R_cl = ifelse(ts_l<(max(temp2$ts_l)*0.33), "Short","Medium"),) %>%
  mutate(R_cl = ifelse(ts_l>(max(temp2$ts_l)*0.66), "Long", R_cl)) %>%
  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long")) %>%
  mutate(Year_f = as.factor(Year))

ggplot(data=temp2, aes(x=Year, y=POHL_Mean)) +
  geom_point(data=temp2, aes(x=Year, y=POHL_Clean, color=R_cl),
             size=0.5, alpha=0.5, shape=1) +
  geom_point(color="Black") +
  geom_line(stat = "smooth", method = "lm", size = 1, alpha = 0.5, color="Black") +
  theme_bw() +
  scale_color_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  labs(y="POHL (mm)",
       color="Range of time series") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  facet_wrap(~Stock_Clean, ncol=3)

ggsave("out/trends/sizemean/timeseries.jpg", width = 10, height = 12)




# Regressions and rate of change ####
sys <- unique(temp2$Stock_Clean)

params <- data.frame()
for(i in 1:length(sys)) {
  sys.choice <- sys[i]
  temp <- temp2 %>% 
    filter(Stock_Clean==sys[i])
  fit <- lm(POHL_Mean ~ Year, dat=temp)
  
  conf <- confint(fit, "Year", level=0.95)
  up <- c(summary(fit)$coefficients[2,1] + conf[1,2])
  low <- c(summary(fit)$coefficients[2,1] - conf[1,2])
  
  x <- data.frame(Stock=sys.choice,
                  Slope=summary(fit)$coefficients[2,1],
                  SE=summary(fit)$coefficients[2,2],
                  pval=summary(fit)$coefficients[2,4],
                  Sig=ifelse(summary(fit)$coefficients[2,4]<0.05,"Y","N"),
                  Sig_al=ifelse(summary(fit)$coefficients[2,4]<0.05,1,0.5),
                  Sig_sz=ifelse(summary(fit)$coefficients[2,4]<0.05,4,2),
                  Sig_sp=ifelse(summary(fit)$coefficients[2,4]<0.05,19,1),
                  Rsq=summary(fit)$r.squared,
                  R_cl=unique(temp$R_cl),
                  upr=up,
                  lwr=low)
  params <- rbind.data.frame(params, x)
}

params <- params %>%
  mutate(Rsq_dec = round(Rsq, 3)) %>%
  mutate(Rsq_lb = ifelse(Rsq_dec<0.1, "<0.1", Rsq_dec)) %>%
  mutate(Rsq_sz = (Rsq)*2) %>%
  arrange(pval) %>%
  mutate(Shp=case_when(Sig=="Y" & 
                         !Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Enhanced (<0.05)",
                       Sig=="N" &
                         !Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Enhanced",
                       Sig=="Y" & 
                         Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Wild/Low enhancement (<0.05)",
                       Sig=="N" & 
                         Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Wild/Low enhancement"))

write.csv(params, "out/trends/sizemean/stats.csv")

params2 <- rel %>%
  mutate(STOCK_NAME = case_when(STOCK_NAME=="Shuswap R Low" ~ "Shuswap R",
                                TRUE ~ STOCK_NAME)) %>%
  mutate(Region = case_when(STOCK_PROD_AREA_CODE %in% c("NCST", "QCI", "SKNA", "NASS") ~ "NC/CC",
                            STOCK_PROD_AREA_CODE %in% c("GSMN", "GSVI", "JNST", "LWFR", "TOMM", "TOMF", "GSMS", "UPFR") ~ "ISC",
                            STOCK_PROD_AREA_CODE %in% c("SWVI", "NWVI") ~ "WCVI",
                            STOCK_PROD_AREA_CODE %in% c("CCST", "RIVR") ~ "NC/CC")) %>%
  select(STOCK_NAME, Region) %>%
  rename("Stock"=STOCK_NAME) %>%
  distinct() %>%
  group_by(Stock) %>%
  right_join(params) %>%
  ungroup() %>%
  mutate(Sig_sz = case_when(Sig_sz==2 ~ 3,
                            TRUE ~ 4))


# Scatter Plots ####

params2

params3<-params2 %>%
  mutate(Stock = fct_reorder(Stock, -Slope))

ggplot(params3,aes(y=Slope, x=Stock, color=R_cl, fill=R_cl,shape=Shp)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1)+
  geom_point(size=2) +
    coord_flip() +
    guides(fill="none")
    


ggplot(params3,aes(y=Slope, x=Stock, color=R_cl, fill=R_cl)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_point(size=2,
             aes(shape=params2$Shp)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  guides(fill="none") +
  scale_y_continuous(breaks=seq(-10,5,5)) +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
  scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  scale_color_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  labs(y = "Change in mean POHL, mm.yr-1",
       color = "Length of time series",
       shape="")

ggsave("out/trends/sizemean/rates.jpg", width = 7, height = 4)

# Boxplot ####

params2 %>%
  mutate(Stock = fct_reorder(Region, -Slope)) %>%
  mutate(Region = fct_relevel(Region, "WCVI", "ISC", "NC/CC")) %>%
  ggplot(aes(y=Slope, x=Region)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_boxplot() +
  geom_jitter(size=2,
              width=0.25,
              alpha=0.5,
              aes(color=params2$R_cl,
                  shape=params2$Shp,
                  fill=params2$R_cl)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  guides(fill="none") +
  scale_y_continuous(breaks=seq(-10,5,5)) +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
  scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  scale_color_manual(values =c("Long"="#2E7D32", "Medium"="#FFB300", "Short"="#BF360C")) +
  labs(y = "Change in mean POHL, mm.yr-1",
       color = "Length of time series",
       shape="")

ggsave("out/trends/sizemean/rates_reg.jpg", width = 7, height = 4)


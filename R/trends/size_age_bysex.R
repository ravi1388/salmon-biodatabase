# Notes ####
# Use this script to conduct regressions on sex-specific time series of size 
# at age and produce summary statistics and plots

# Packages ####
library(tidyverse)
library(lemon)
library(tidytext)

# Data ####
rm(list=ls())
dat <- read.csv("dat/processed/cnbiodata_clean.csv")
rel <- read.csv("dat/raw/sepreleases.csv")

# Variables
t_pt <- 9
t_yr <- 4
t_age <- 1


# Filtering ####
dat <- dat %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  filter(Sex_Clean %in% c("Male",
                          "Female")) %>%
  filter(!is.na(Stock_Clean)) %>%
  filter(!is.na(POHL_Clean)) %>%
  select(Year, POHL_Clean, OceanAge, Stock_Clean, Sex_Clean)

df <- dat %>%
  group_by(Stock_Clean, OceanAge, Sex_Clean, Year) %>%
  summarise(n_pt=n()) %>%
  filter(n_pt > t_pt) %>%
  group_by(Stock_Clean, OceanAge, Sex_Clean, Year) %>%
  right_join(dat) %>%
  filter(!is.na(n_pt))

df2 <- df %>%
  select(Stock_Clean, Year, Sex_Clean, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean, Sex_Clean, Year) %>%
  summarise(n_age=n()) %>%
  filter(n_age > t_age)  %>%
  group_by(Stock_Clean, Sex_Clean, Year) %>%
  right_join(df) %>%
  filter(!is.na(n_age))

df3 <- df2 %>%
  select(Stock_Clean, Year, Sex_Clean, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean, OceanAge, Year) %>%
  summarise(n_sex=n()) %>%
  filter(n_sex > 1)  %>%
  group_by(Stock_Clean, OceanAge, Year) %>%
  right_join(df) %>%
  filter(!is.na(n_sex))

df4 <- df3 %>%
  select(Stock_Clean, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, OceanAge) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr > t_yr) %>%
  group_by(Stock_Clean, OceanAge) %>%
  right_join(df) %>%
  filter(!is.na(n_yr))

df4 <- df4 %>%
  select(Stock_Clean, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, OceanAge) %>%
  summarise(n_yr=n()) %>%
  right_join(df4)

sex <- unique(df4$Sex_Clean)

for(i in 1:2) {
  temp <- df4 %>%
    filter(Sex_Clean==sex[i])
  
  temp %>%
    ggplot(aes(x=Year, y=POHL_Clean, color=OceanAge)) +
    geom_point(alpha=0.1, 
               size=0.5) +
    geom_line(stat = "smooth", method = "lm", size = 1) +
    theme_bw() +
    scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    labs(y="POHL (mm)", color = "Ocean age") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~Stock_Clean,
               ncol=3) +
    theme(legend.position = "bottom")
  
  ggsave(paste("out/trends/sizeage/timeseries_",sex[i],".jpg", sep=""), width = 8, height = 8)
}


# Regressions and rate of change - All ####
age <- unique(df4$OceanAge)
sys <- unique(df4$Stock_Clean)
sex <- unique(df4$Sex_Clean)

params <- data.frame()
i=j=k=1
for(i in 1:length(sys)) {
  sys.choice <- sys[i]
  for(j in 1:length(age)) {
    age.choice <- age[j]
    for(k in 1:2) {
      sex.choice <- sex[k] 
      temp <- df4 %>% 
        filter(Stock_Clean==sys[i] & OceanAge==age[j] & Sex_Clean==sex[k])
      if(nrow(temp)>0) {
        fit <- lm(POHL_Clean ~ Year, dat=temp)
        conf <- confint(fit, "Year", level=0.95)
        up <- c(summary(fit)$coefficients[2,1] + conf[1,2])
        low <- c(summary(fit)$coefficients[2,1] - conf[1,2])
        x <- data.frame(Stock=sys.choice,
                        Age=age.choice,
                        Slope=summary(fit)$coefficients[2,1],
                        Up95=up,
                        Low95=low,
                        SE=summary(fit)$coefficients[2,2],
                        pval=summary(fit)$coefficients[2,4],
                        Sig=ifelse(summary(fit)$coefficients[2,4]<0.05,"Y","N"),
                        Sig_al=ifelse(summary(fit)$coefficients[2,4]<0.05,1,0.5),
                        Sig_sz=ifelse(summary(fit)$coefficients[2,4]<0.05,4,2),
                        Sig_sp=ifelse(summary(fit)$coefficients[2,4]<0.05,19,1),
                        Sex_sp=case_when(sex.choice=="Female" ~ 17,
                                         sex.choice=="Male" ~ 15),
                        Rsq=summary(fit)$r.squared,
                        N=unique(temp$n_yr),
                        Sex=sex.choice)
        params <- rbind.data.frame(params, x)
      }
    }
  }
}

params <- params %>%
  mutate(Rsq_dec = round(Rsq, 3)) %>%
  mutate(Rsq_sz = (Rsq_dec)*2) %>%
  arrange(Age, pval) %>%
  mutate(R_cl = ifelse(N<(max(params$N)*0.33), "Short","Medium"),) %>%
  mutate(R_cl = ifelse(N>(max(params$N)*0.66), "Long", R_cl)) %>%
  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long"))

write.csv(params, "out/trends/sizeage/stats_bysex.csv")

# Add rates of change from calculated from combined data

temp <- read.csv("out/trends/sizeage/stats.csv")
params2 <- temp %>%
  select(Stock_Clean, Age, Slope, upr, lwr, SE, pval, Sig, Sig_al,
         Sig_sz,  Sig_sp, Sex_sp, Rsq, N, Rsq_dec, Rsq_sz, R_cl, Sex) %>%
  rename("Stock"=Stock_Clean) %>%
  bind_rows(params)

# Add Region

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
  right_join(params2) %>%
  ungroup() %>%
  mutate(Sig_sz = case_when(Sig_sz==2 ~ 3,
                            TRUE ~ 4)) %>%
  mutate(Shp=case_when(Sig=="Y" & 
                         !Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Enhanced (<0.05)",
                       Sig=="N" &
                         !Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Enhanced",
                       Sig=="Y" & 
                         Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Wild/Low enhancement (<0.05)",
                       Sig=="N" & 
                         Stock %in% c("Stuart R","Nechako R","Harrison R") ~ "Wild/Low enhancement"))

# Slope plot ####

params2 %>%
  mutate(Stock = reorder_within(Stock, -Slope, Sex)) %>%
  ggplot(aes(y=Slope, x=Stock, color=Age, fill=Age)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_point(size=2,
             aes(shape=Shp)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~Sex,
             scales="free_y",
             ncol = 1) +
  scale_x_reordered() +
  coord_flip() +
  scale_y_continuous(breaks=seq(-10,5,1)) +
  labs(y = "Change in POHL, mm.yr-1",
       color = "Ocean Age",
       shape="") +
  guides(fill="none") +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
  scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
  scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA"))

ggsave("out/trends/sizeage/rates_bysex.jpg", width = 10, height = 4)


# Boxplot ####

params2 %>%
  mutate(Stock = reorder_within(Region, -Slope, Sex)) %>%
  ggplot(aes(y=Slope, x=Region, color=Age)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_boxplot() +
  geom_point(size=2,
             aes(shape=Shp,
                 fill=Age),
             alpha=0.5,
             position=position_jitterdodge(jitter.width = 0.1,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_grid(Sex~Age) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(breaks=seq(-10,11,5)) +
  labs(y = "Change in POHL, mm.yr-1",
       color = "Ocean Age",
       shape="") +
  guides(fill="none") +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
  scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
  scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
  theme(legend.position = "bottom", 
        legend.box="vertical",
        legend.margin=margin())

ggsave("out/trends/sizeage/rates_reg_bysex.jpg", width = 10, height = 4)

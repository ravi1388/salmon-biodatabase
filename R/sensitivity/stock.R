# Notes ####
# Use this script to conduct a sensitivity analysis on the use of Stock (ONLY 
# CWT derived stock of origin) and Stock2 (BOTH CWT derived stock of origin and 
# sample site)

# Packages ####
library(tidyverse)
library(lemon)
library(tidytext)

# Data ####
rm(list=ls())
biodat <- read.csv("dat/processed/cnbiodata_clean.csv")

# Variables ####
t_pt <- 9
t_yr <- 4
t_age <- 1
stk <- c("Big Qualicum R", "Chilliwack R", "Nechako R", "Nitinat R" ,
         "Puntledge R", "Quinsam R", "Robertson Cr", "Stuart R", "Atnarko R",
         "Babine R", "Burman R", "Capilano R", "Chehalis R", "Chemainus R",
         "Conuma R", "Cowichan R", "Harrison R", "Kincolith R", "L Qualicum R",
         "Nahmint R", "Sarita R", "Shuswap R" )


# Stock ####
dat <- biodat %>%
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

df4 <- df3 %>%
  select(Stock_Clean, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean, OceanAge) %>%
  summarise(n_yr=n()) %>%
  right_join(df3) %>%
  rename("Stock"=Stock_Clean) %>%
  mutate(Indicator="Stock")

# Stock 2 ####
dat <- biodat %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  filter(!is.na(POHL_Clean)) %>%
  select(Year, POHL_Clean, OceanAge, Stock_Clean2)

df <- dat %>%
  group_by(Stock_Clean2, OceanAge, Year) %>%
  summarise(n_pt=n()) %>%
  filter(n_pt > t_pt) %>%
  group_by(Stock_Clean2, OceanAge, Year) %>%
  right_join(dat) %>%
  filter(!is.na(n_pt))

df2 <- df %>%
  select(Stock_Clean2, Year, OceanAge) %>%
  distinct() %>%
  group_by(Stock_Clean2, Year) %>%
  summarise(n_age=n()) %>%
  filter(n_age > t_age)  %>%
  group_by(Stock_Clean2, Year) %>%
  right_join(df) %>%
  filter(!is.na(n_age))

df3 <- df2 %>%
  select(Stock_Clean2, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean2, OceanAge) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr > t_yr) %>%
  group_by(Stock_Clean2, OceanAge) %>%
  right_join(df) %>%
  filter(!is.na(n_yr))

df3 <- df3 %>%
  select(Stock_Clean2, OceanAge, Year) %>%
  distinct() %>%
  group_by(Stock_Clean2, OceanAge) %>%
  summarise(n_yr=n()) %>%
  right_join(df3) %>%
  rename("Stock"=Stock_Clean2) %>%
  mutate(Indicator="Stock2") %>%
  bind_rows(df4)

ind <- unique(df3$Indicator)

for(i in 1:2) {
  df3 %>%
    filter(Indicator==ind[i]) %>%
    ggplot(aes(x=Year, y=POHL_Clean, color=OceanAge)) +
    geom_point(alpha=0.1, 
               size=0.5) +
    geom_line(stat = "smooth", method = "lm", size = 1) +
    theme_bw() +
    scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    labs(y="POHL (mm)", color = "Ocean age") +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~Stock,
               ncol=3) +
    theme(legend.position = "bottom")
  
  ggsave(paste("out/sensitivity/stock/sizeage_",ind[i],".jpg", sep=""), width=8, height=12)
  
}

# Regressions and rate of change - All ####
age <- unique(df3$OceanAge)
sys <- unique(df3$Stock)
ind <- unique(df3$Indicator)

params <- data.frame()
i=j=k=1

for(k in 1:length(ind)) {
  for(i in 1:length(sys)) {
    for(j in 1:length(age)) {
      sys.choice <- sys[i]
      age.choice <- age[j]
      ind.choice <- ind[k]
      
      temp <- df3 %>% 
        filter(Stock==sys[i] & OceanAge==age[j] & Indicator==ind[k])
      if(nrow(temp)>0) {
        fit <- lm(POHL_Clean ~ Year, dat=temp)
        conf <- confint(fit, "Year", level=0.95)
        up <- c(summary(fit)$coefficients[2,1] + conf[1,2])
        low <- c(summary(fit)$coefficients[2,1] - conf[1,2])
        x <- data.frame(Stock=sys.choice,
                        Age=age.choice,
                        Indicator=ind.choice,
                        Slope=summary(fit)$coefficients[2,1],
                        Up95=up,
                        Low95=low,
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
}

params <- params %>%
  mutate(Rsq_dec = round(Rsq, 3)) %>%
  mutate(Rsq_sz = (Rsq_dec)*2) %>%
  arrange(Age, pval) %>%
  mutate(R_cl = ifelse(N<(max(params$N)*0.33), "Short","Medium"),) %>%
  mutate(R_cl = ifelse(N>(max(params$N)*0.66), "Long", R_cl)) %>%
  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long")) %>%
  mutate(Sex = "Both")


write.csv(params, "out/sensitivity/stock/sizeage_stats.csv")


# Scatter plot ####

params %>%
  mutate(Stock = reorder_within(Stock, -Slope, Age)) %>%
  ggplot(aes(y=Slope, x=Stock, color=Indicator)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_point(size=3,
             shape=params$Sig_sp) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(breaks=seq(-10,5,5)) +
  facet_wrap(~Age,
             scales = "free_y",
             ncol = 2) +
  labs(y = "Change in POHL, mm.yr-1",
       color = "Ocean age",
       x="Stock") +
  scale_color_manual(values = c("Stock"="#F4511E","Stock2"="#26C6DA")) +
  theme(legend.position = "bottom", legend.box="vertical", legend.margin=margin())

ggsave("out/sensitivity/stock/sizeage_rates.jpg", width = 9, height = 8)

stock <- unique(df3$Stock)

write.csv(stock, "out/sensitivity/stock/stock.csv")
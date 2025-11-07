# Notes ####
# Use this script to estimate mean age from age compositions calculate from 
# EPADS Obs-Est-Exp Returns, conduct regressions on the resulting time series 
# and produce summary statistics and plots

# Packages ####
rm(list=ls())

library(tidyverse)
library(lemon)
library(tidytext)

# Data ####
dat <- read.csv("dat/processed/comps_oee.csv")
rel <- read.csv("dat/raw/sepreleases.csv")

unique(dat$Stock_Clean)

# Variables ####
t_pt <- 19
t_yr <- 4
t_age <- 2


# Filtering ####
dat2 <- dat %>%
  mutate(Indicator = ifelse(Indicator=="Estimated", "EPAD - Estimated returns",
                            ifelse(Indicator=="Observed", 
                                   "EPAD - Observed returns",
                                   "EPAD - Expanded returns"))) %>%
  filter(!Stock_Clean=="Yukon R") %>%
  filter(OceanAge %in% c("Ocean-2",
                         "Ocean-3",
                         "Ocean-4")) %>%
  mutate(Age = ifelse(OceanAge=="Ocean-4", 5,
         ifelse(OceanAge=="Ocean-3", 4, 3))) %>%
  filter(!is.na(Stock_Clean)) %>%
  select(Indicator, Year, Age, Stock_Clean, 
         OceanAge, p_age, Counts_age, Counts_sum)


dat3 <- dat2 %>%
  select(Indicator, Stock_Clean, Year, Age) %>%
  distinct() %>%
  group_by(Indicator, Stock_Clean, Year) %>%
  summarise(n_age=n()) %>%
  filter(n_age>t_age) %>%
  group_by(Indicator, Stock_Clean, Year) %>%
  right_join(dat2) %>%
  filter(!is.na(n_age))

dat4 <- dat3 %>%
  select(Indicator, Stock_Clean, Year) %>%
  distinct() %>%
  group_by(Indicator, Stock_Clean) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr > t_yr) %>%
  group_by(Indicator, Stock_Clean) %>%
  right_join(dat3) %>%
  filter(!is.na(n_yr)) %>%
  group_by(Indicator, Year, Stock_Clean) %>%
  summarise(Age_mean = sum(Age*Counts_age)/Counts_sum) %>%
  right_join(dat3)

sys <- unique(dat4$Stock_Clean)
temp2 <- data.frame()
i=1
for(i in 1:length(sys)) {
  
  temp <- filter(dat4, Stock_Clean==sys[i])
  temp <- temp %>%
    mutate(ts_l = max(temp$Year)-min(temp$Year))
  temp2 <- rbind.data.frame(temp2, temp)
  
}

#df <- temp2  %>%
#  ungroup() %>%
#  mutate(R_cl = ifelse(ts_l<(max(temp2$ts_l)*0.33), "Short","Medium"),) %>%
#  mutate(R_cl = ifelse(ts_l>(max(temp2$ts_l)*0.66), "Long", R_cl)) %>%
#  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long"))

df <- temp2  %>%
  ungroup() %>%
  mutate(R_cl = ifelse(ts_l<(max(temp2$ts_l)*0.33), "Short","Medium"),) %>%
  mutate(R_cl = ifelse(ts_l>(max(temp2$ts_l)*0.66), "Long", R_cl)) %>%
  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long"))%>%
  filter(Stock_Clean!="Quesnel R")

unique(df$Stock_Clean)

#Ravi original code
#df2 <- df %>%
#  select(Stock_Clean, Year, Age, OceanAge, R_cl, Counts_age, Indicator) %>%
#  distinct()

#andy addition to remove Quesnel
df2 <- df %>%
  select(Stock_Clean, Year, Age, OceanAge, R_cl, Counts_age, Indicator) %>%
  filter(Stock_Clean!="Quesnel R")%>%
  distinct()

inds <- unique(df$Indicator)

j=1

for(j in 1:length(inds)) {
  ind.choice <- inds[j]
  
  df3 <- df %>%
    filter(Indicator==ind.choice)
  
  df4 <- df2 %>%
    filter(Indicator==ind.choice)
  
  ggplot(data=df3, aes(x=Year, y=Age_mean)) +
    geom_point(data=df4, aes(x=Year, y=Age, color=R_cl, size=Counts_age),
               alpha=0.5, shape=19) +
    geom_point(color="Black") +
    geom_line(stat = "smooth", method = "lm", size = 1, alpha = 0.5, color="Black") +
    theme_bw() +
    scale_color_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
    labs(y="Age, Years",
         color="Length of time series",
         size=paste("",ind.choice,"")) +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(angle = 90),
          legend.position = "bottom", legend.box = "vertical") +
    facet_wrap(~Stock_Clean, ncol=3)
  
  ggsave(paste("out/trends/agemean/timeseries_",ind.choice,".jpg", sep=""), width = 15, height = 15)
 
  
  # Regressions and rate of change - All ####
  sys <- unique(df3$Stock_Clean)
  
  params <- data.frame()
  i=1
  for(i in 1:length(sys)) {
    sys.choice <- sys[i]
    temp <- df3 %>% 
      filter(Stock_Clean==sys[i])
    if(nrow(temp)>0) {
      fit <- lm(Age_mean ~ Year, dat=temp)
      
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
  }
  
  params <- params %>%
    mutate(Rsq_dec = round(Rsq, 3)) %>%
    mutate(Rsq_sz = (Rsq_dec)*2) %>%
    arrange(Slope) %>%
    mutate(Shp=case_when(Sig=="Y" & 
                           !Stock %in% c("Kitsumkalum R","Harrison R") ~ "Enhanced (<0.05)",
                         Sig=="N" &
                           !Stock %in% c("Kitsumkalum R","Harrison R") ~ "Enhanced",
                         Sig=="Y" & 
                           Stock %in% c("Kitsumkalum R","Harrison R") ~ "Wild/Low enhancement (<0.05)",
                         Sig=="N" & 
                           Stock %in% c("Kitsumkalum R","Harrison R") ~ "Wild/Low enhancement"))
  
  write.csv(params, paste("out/trends/agemean/stats_",ind.choice,".csv", sep=""))
  
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
  
  # Scatter plots ####
  params2 %>%
    mutate(Stock = fct_reorder(Stock, -Slope)) %>%
    ggplot(aes(y=Slope, x=Stock, color=R_cl, fill=R_cl)) +
    geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
    geom_point(size=2,
               aes(shape=params2$Shp)) +
    theme(panel.grid.minor = element_blank()) +
    theme_bw() +
    coord_flip() +
    guides(fill="none") +
    labs(y = paste("Change in mean age (",ind.choice,"), yrs.yr-1", sep=""),
         color = "Length of time series",
         shape="Enhancement/Significance") +
    scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                  "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
    scale_color_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
    scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19"))
  
  ggsave(paste("out/trends/agemean/rates_",ind.choice,".jpg", sep=""), width = 7, height = 4)
  
  
  # Region Boxplots ####
  params2 %>%
    mutate(Region = fct_relevel(Region, "WCVI", "ISC", "NC/CC")) %>%
    ggplot(aes(y=Slope, x=Region)) +
    geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(size=2,
                width=0.25,
                alpha=0.5,
                aes(color=params2$R_cl,
                    shape=params2$Shp)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_flip() +
    scale_x_reordered() +
    labs(y = paste("Change in mean age (",ind.choice,"), yrs.yr-1", sep=""),
         shape="",
         color = "Length of time series") +
    scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                  "Wild/Low enhancement (<0.05)"=17, "Wild/Low enhancement"=2)) +
    scale_color_manual(values =c("Long"="#2E7D32", "Medium"="#FFB300", "Short"="#BF360C")) +
    scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19"))
  
  ggsave(paste("out/trends/agemean/rates_reg_",ind.choice,".jpg", sep=""), width = 7, height = 4)

}

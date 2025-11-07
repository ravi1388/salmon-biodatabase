# Notes ####
# Use this script to conduct logistic regressions of age compositions calculated from 
# EPADS Obs-Est-Exp Returns and produce summary statistics and plots

# Packages
library(tidyverse)
library(tidytext)
library(data.table)
library(DescTools)

# Data
rm(list=ls())
dat <- read.csv("dat/processed/comps_oee.csv")
rel <- read.csv("dat/raw/sepreleases.csv")


df <- dat %>%
  mutate(Indicator = ifelse(Indicator=="Estimated", "EPADS - Estimated returns",
                            ifelse(Indicator=="Observed", 
                                   "EPADS - Observed returns",
                                   "EPADS - Expanded returns"))) %>%
  select(Stock_Clean, Year, Indicator, Year, OceanAge, p_age) %>%
  filter(!Stock_Clean=="Yukon R")

# Logistic regression ####

# logreg_summary: summarize logistic regression as in Table 3 of Lewis et al.
# Since the log odds ratios are infinite at 0 and 1, only use the subset with
# proportions in (0, 1). alpha is the significance level.
logreg_summary = function(p, year, alpha) {
  i = which(p > 0.0 & p < 1.0)
  n = length(i)
  if (n < 2L)
    return(list(N = n, Start = NA_integer_, End = NA_integer_,
                Intercept = NA_real_, Slope = NA_real_, StdErr = NA_real_,
                Z = NA_real_, P = NA_real_, Significant = NA))
  mod = lm(qlogis(p[i]) ~ year[i])
  slope = coef(mod)[[2L]]
  se = sqrt(vcov(mod)[2L, 2L])
  z = slope/se
  rsq = PseudoR2(mod, which = "CoxSnell")
  conf = confint(mod)
  pval = 2.0*pt(abs(z), df.residual(mod), lower.tail = FALSE)
  upr = slope+conf[1,2]
  lwr = slope-conf[1,2]
  list(N = n, Start = min(year[i]), End = max(year[i]),
       Intercept = coef(mod)[[1L]], Slope = slope, StdErr = se,
       Z = z, P = pval, Significant = pval < alpha, Rsq = rsq,
       Upr=upr, Lwr=lwr)
}

# logreg_line: construct data for the trendlines as in Figure 4 of Lewis et
# al. using the regression summaries.
logreg_line = function(summary, n = 101L) {
  if (is.na(summary[["Start"]]))
    return(NULL)
  x = seq(summary[["Start"]], summary[["End"]], length.out = n)
  y = plogis(summary[["Intercept"]] + summary[["Slope"]]*x)
  list(x = x, y = y, significant = summary[["Significant"]])
}

inds <- unique(df$Indicator)

i=1
#set i=1 for estimated returns
for(i in 1:3) {
  
  ind.choice <- inds[i]
  df2 <- df %>%
    filter(Indicator==ind.choice) 
  
  df2 <- df2 %>%
    select(Stock_Clean, OceanAge, Year) %>%
    distinct() %>%
    group_by(Stock_Clean, OceanAge) %>%
    summarise(n=n()) %>%
    filter(n>4) %>%
    select(-n) %>%
    group_by(Stock_Clean, OceanAge) %>%
    left_join(df2)
  
  comps <- setDT(df2)
  
  # Only select those stock/Year combinations with complete age distributions
  age_classes = comps[, .(Age = unique(OceanAge)),
                      .(Stock = Stock_Clean)]
  
  comps %>%
    select(OceanAge, Stock_Clean) %>%
    distinct()
  
  age_sample = comps[,
                     .(Use = all(age_classes[Stock_Clean, Age, on = "Stock"] %in% OceanAge)),
                     .(Stock_Clean, Year)
  ]
  age_sample = age_sample[Use == TRUE, c("Stock_Clean", "Year")]
  age_sample = comps[age_sample, on = c("Stock_Clean", "Year")]
  
  logregs = age_sample[, logreg_summary(p_age, Year, 0.05),
                       .(Stock_Clean, OceanAge)]
  
  linetypes = c(
    "Ocean-1" = "dashed",
    "Ocean-2" = "dotted",
    "Ocean-3" = "solid",
    "Ocean-4" = "dotdash",
    "Ocean-5" = "twodash"
  )
  
  trends = logregs[, logreg_line(.SD), .(Stock_Clean, OceanAge)]
  trends[, linetype := linetypes[OceanAge]]
  
  # Replication of Figure 4 in Lewis et al.
  
  # Cairo(8.0, 7.0, "chinook_age_comps.png", units = "in", dpi = 128.0)
  
  age_sample %>%
    ggplot(aes(Year, p_age, color = OceanAge)) +
    facet_wrap(vars(Stock_Clean), ncol = 4) +
    geom_point(alpha=0.5,
               size=2) +
    geom_line(aes(x, y, linetype = linetype), 
              color = "red", size = 0.75,
              trends[significant == TRUE], show.legend = TRUE) +
    geom_line(aes(x, y, linetype = linetype),
              color = "black", size = 0.75,
              trends[significant == FALSE], show.legend = TRUE) +
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    labs(x="Year",
         y=paste("Age composition (",ind.choice,")", sep=""),
         color="Ocean Age",
         shape="Ocean Age") +
    scale_linetype_manual(name = "Ocean Age",
                          values = c("dashed", "dotted",
                                     "solid", "dotdash",
                                     "twodash"),
                          labels = c("Ocean-1",
                                     "Ocean-2",
                                     "Ocean-3",
                                     "Ocean-4",
                                     "Ocean-5"))
  
  ggsave(paste("out/trends/agecomp/timeseries_",ind.choice,".jpg", sep=""), width = 10, height = 8)
  
  stat_df <- as.data.frame(logregs)
  
  stat_df <- stat_df %>%
    filter(!is.na(Significant)) %>%
    select(Stock_Clean, OceanAge, N, Intercept, Slope, StdErr, Z, P, Significant) %>%
    mutate(Indicator=ind.choice)
  
  write.csv(stat_df, paste("out/trends/agecomp/stats_",ind.choice,".csv", sep=""))
  
  # Rates ####
  
  params <- stat_df %>%
    mutate(Sig_sp=ifelse(Significant==TRUE,19,1)) %>%
    mutate(Slope_pc = Slope*100) %>%
    mutate(R_cl = ifelse(N<(max(stat_df$N)*0.33), "Short","Medium"),) %>%
    mutate(R_cl = ifelse(N>(max(stat_df$N)*0.66), "Long", R_cl)) %>%
    mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long"))
  
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
    mutate(Shp=case_when(Significant=="TRUE" & 
                           !Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Enhanced (<0.05)",
                         Significant=="FALSE" &
                           !Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Enhanced",
                         Significant=="TRUE" & 
                           Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Wild/Low enhancement (<0.05)",
                         Significant=="FALSE" & 
                           Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Wild/Low enhancement")) %>%
    ungroup()
  
  # Scatter plots ####
  params2 %>%
    mutate(Stock_Clean = fct_reorder(Stock_Clean, -Slope)) %>%
    ggplot(aes(y=Slope_pc, x=Stock_Clean, color=OceanAge, fill=OceanAge)) +
    geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
    geom_point(size=2,
               aes(shape=params2$Shp)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_flip() +
    scale_x_reordered() +
    labs(y = paste("Change in age composition (",ind.choice,"), percent yr-1", sep=""),
         x = "Stock",
         color = "Ocean Age",
         shape="") +
    guides(fill="none") +
    scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                  "Wild/Low enhancement (<0.05)"=24, "Wild/Low enhancement"=2))  +
    scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA"))
  
  ggsave(paste("out/trends/agecomp/rates_",ind.choice,".jpg", sep = ""), width=7, height=4)
  
  
  # Boxplot ####
  
  params2%>%mutate(Region = fct_relevel(Region, "WCVI", "ISC", "NC/CC"))%>%
    ggplot(aes(y=Slope_pc, x=Region, color=OceanAge)) +
    geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
    geom_boxplot(outlier.shape=NA) +
    geom_point(size=2,
               aes(shape=Shp,
                   fill=OceanAge),
               alpha=0.75,
               position=position_jitterdodge(jitter.width = 0.1,
                                             jitter.height = 0,
                                             dodge.width = 0.75,
                                             seed = NA)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_flip() +
    guides(fill="none") +
    facet_wrap(~OceanAge, ncol=1) +
    labs(y = paste("Change in age composition (",ind.choice,"), percent yr-1", sep=""),
         x = "Stock",
         color = "Age",
         shape="") +
    scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                  "Wild/Low enhancement (<0.05)"=24, "Wild/Low enhancement"=2))  +
    scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    theme(legend.position = "bottom", 
          legend.box="vertical",
          legend.margin=margin())
  
  ggsave(paste("out/trends/agecomp/rates_reg_",ind.choice,".jpg", sep = ""), width=7, height=7)
  
}

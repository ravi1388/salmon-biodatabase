# Notes ####
# Use this script to conduct logistic regressions of sex-specific age 
# compositions calculated from EPADS Obs-Est-Exp Returns and produce summary
# statistics and plots

# Packages ####
library(tidyverse)
library(tidytext)
library(data.table)
library(DescTools)

# Data ####
rm(list=ls())
dat <- read.csv("dat/processed/comps_oee_bysex.csv")
rel <- read.csv("dat/raw/sepreleases.csv")

df <- dat %>%
  mutate(Indicator = ifelse(Indicator=="Estimated", "EPAD - Estimated returns",
                            ifelse(Indicator=="Observed", 
                                   "EPAD - Observed returns",
                                   "EPAD - Expanded returns"))) %>%
  filter(!Stock_Clean=="Yukon R") %>%
  select(Stock_Clean, Year, Indicator, Year, OceanAge, Sex_Clean, p_age)

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
  pval = 2.0*pt(abs(z), df.residual(mod), lower.tail = FALSE)
  list(N = n, Start = min(year[i]), End = max(year[i]),
       Intercept = coef(mod)[[1L]], Slope = slope, StdErr = se,
       Z = z, P = pval, Significant = pval < alpha, Rsq = rsq)
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
sex <- unique(df$Sex_Clean)

for(i in 1:3) {
  ind.choice <- inds[i]
  stat_df <- data.frame()
  
  df2 <- df %>%
    filter(Indicator==ind.choice)
  
  for(j in 1:2) {
    sex.choice <- sex[j]
    
    df3 <- df2 %>%
      filter(Sex_Clean == sex.choice)
    
    df3 <- df3 %>%
      select(Stock_Clean, OceanAge, Year) %>%
      distinct() %>%
      group_by(Stock_Clean, OceanAge) %>%
      summarise(n=n()) %>%
      filter(n>4) %>%
      select(-n) %>%
      group_by(Stock_Clean, OceanAge) %>%
      left_join(df3)
    
    comps <- setDT(df3)
    
    # Only select those stock/Year combinations with complete age distributions
    age_classes = comps[, .(Age = unique(OceanAge)),
                        .(Stock = Stock_Clean)]
    
    comps %>%
      select(OceanAge, Stock_Clean) %>%
      distinct()
    
    age_sample  = comps[,
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
      scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
      labs(x="Year",
           y=paste("Age composition (",ind.choice,")", sep=""),
           color="Ocean Age",
           shape="Ocean Age") +
      scale_linetype_manual(name = "Ocean Age",
                            values = c("dashed", "dotted",
                                       "solid"),
                            labels = c("Ocean-2",
                                       "Ocean-3",
                                       "Ocean-4"))
    
    ggsave(paste("out/trends/agecomp/timseries_",sex.choice,"_",ind.choice,".jpg", sep=""), width = 10, height = 8)
    
    temp <- as.data.frame(logregs)
    
    temp <- temp %>%
      filter(!is.na(Significant)) %>%
      select(Stock_Clean, OceanAge, N, Intercept, Slope, StdErr, Z, P, Significant) %>%
      mutate(Indicator=ind.choice) %>%
      mutate(Sex=sex.choice)
    
    stat_df <- rbind.data.frame(stat_df, temp)
    
  }
  
  write.csv(stat_df, paste("out/trends/agecomp/stats_bysex_",ind.choice,".csv", sep=""))
  
  # Add combined slopes to results
  temp <- read.csv(paste("out/trends/agecomp/stats_",ind.choice,".csv", sep=""))
  params <- temp %>%
    mutate(Sex = "Both") %>%
    select("Stock_Clean", "OceanAge", "N", "Intercept", "Slope",
           "StdErr", "Z", "P", "Significant", "Indicator", "Sex") %>%
    filter(!Stock_Clean=="Yukon R") %>%
    bind_rows(stat_df)
  
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
    mutate(Shp=case_when(Significant=="TRUE" & 
                           !Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Enhanced (<0.05)",
                         Significant=="FALSE" &
                           !Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Enhanced",
                         Significant=="TRUE" & 
                           Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Wild/Low enhancement (<0.05)",
                         Significant=="FALSE" & 
                           Stock_Clean %in% c("Kitsumkalum R","Harrison R") ~ "Wild/Low enhancement")) %>%
    ungroup() %>%
    mutate(Sig_sp=ifelse(Significant==TRUE,19,1)) %>%
    mutate(Slope_pc = Slope*100) %>%
    mutate(R_cl = ifelse(N<(max(stat_df$N)*0.33), "Short","Medium"),) %>%
    mutate(R_cl = ifelse(N>(max(stat_df$N)*0.66), "Long", R_cl)) %>%
    mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long"))
  
  # Scatter plots ####
  params2 %>%
    mutate(Stock_Clean = reorder_within(Stock_Clean, -Slope, Sex)) %>%
    ggplot(aes(y=Slope_pc, x=Stock_Clean, color=OceanAge, fill=OceanAge)) +
    geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
    geom_point(size=2,
               aes(shape=Shp)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_flip() +
    facet_wrap(~Sex,
               scales = "free_y",
               ncol=1) +
    scale_x_reordered() +
    labs(y = paste("Change in age composition (",ind.choice,"), percent yr-1", sep=""),
         x = "Stock",
         color = "Ocean age",
         shape="") +
    guides(fill="none") +
    scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                  "Wild/Low enhancement (<0.05)"=24, "Wild/Low enhancement"=2))  +
    scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA"))
  
  ggsave(paste("out/trends/agecomp/rates_bysex_",ind.choice,".jpg", sep = ""), width=8, height=7)
  
  
  # Box plots ####
  
  params2 %>%
    mutate(Region = fct_relevel(Region, "WCVI", "ISC", "NC/CC")) %>%
    ggplot(aes(y=Slope_pc, x=Region, color=OceanAge)) +
    geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(size=2,
               aes(shape=Shp,
                   fill=OceanAge),
               alpha=0.5,
               position=position_jitterdodge(jitter.width = 0.1,
                                             jitter.height = 0,
                                             dodge.width = 0.75,
                                             seed = NA)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_flip() +
    facet_grid(Sex~OceanAge) +
    guides(fill="none") +
    labs(y = paste("Change in age composition (",ind.choice,"), percent yr-1", sep=""),
         x = "Stock",
         color = "Ocean age",
         shape="") +
    scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                  "Wild/Low enhancement (<0.05)"=24, "Wild/Low enhancement"=2))  +
    scale_fill_manual(values =c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
    theme(legend.position = "bottom", 
          legend.box="vertical",
          legend.margin=margin())
  
  ggsave(paste("out/trends/agecomp/rates_reg_bysex_",ind.choice,".jpg", sep = ""), width=8, height=5)
  
}

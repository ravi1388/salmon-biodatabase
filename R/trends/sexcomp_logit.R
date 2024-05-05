# Notes ####
# Use this script to conduct logistic regressions of sex compositions calculated from 
# EPADS River Returns and produce summary statistics and plots

# Packages
library(lemon)
library(tidytext)
library(tidyverse)
library(data.table)
library(DescTools)

# Data
rm(list=ls())
dat <- read.csv("dat/processed/comps_rr.csv")
rel <- read.csv("dat/raw/sepreleases.csv")
df <- dat %>%
  filter(!Stock_Clean %in% c("Yukon R", "Salloomt R")) %>%
  select(Stock_Clean, Year, Sex_Clean, Indicator, p, n_yr)


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
  mod = glm(qlogis(p[i]) ~ year[i])
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


stk <- unique(df$Stock_Clean)
comps <- setDT(df)

# Only select those stock/Year combinations with complete Sex distributions
sex_classes = comps[, .(Sex = unique(Sex_Clean)),
                    .(Stock = Stock_Clean)]

comps %>%
  select(Sex_Clean, Stock_Clean) %>%
  distinct()

Sex_sample = comps[,
                   .(Use = all(sex_classes[Stock_Clean, Sex, on = "Stock"] %in% Sex_Clean)),
                   .(Stock_Clean, Year)
]
Sex_sample = Sex_sample[Use == TRUE, c("Stock_Clean", "Year")]
Sex_sample = comps[Sex_sample, on = c("Stock_Clean", "Year")]


logregs = Sex_sample[, logreg_summary(p, Year, 0.05),
                     .(Stock_Clean, Sex_Clean)]

linetypes = c(
  "Male" = "dashed",
  "Female" = "solid"
)

trends = logregs[, logreg_line(.SD), .(Stock_Clean, Sex_Clean)]
trends[, linetype := linetypes[Sex_Clean]]

# Replication of Figure 4 in Lewis et al.

# Cairo(8.0, 7.0, "chinook_Sex_comps.png", units = "in", dpi = 128.0)

Sex_sample %>%
  ggplot(aes(Year, p, color = Sex_Clean)) +
  facet_wrap(vars(Stock_Clean), ncol = 3) +
  geom_point(alpha=0.5,
             size=1) +
  geom_line(aes(x, y, linetype = linetype), color = "red",
            trends[significant == TRUE], show.legend = TRUE) +
  geom_line(aes(x, y, linetype = linetype), color = "black",
            trends[significant == FALSE], show.legend = TRUE) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x="Year",
       y="Sex composition (EPAD - River returns)",
       color="Sex",
       shape="Sex") +
  scale_linetype_manual(name = "Sex",
                        values = c("dashed", "solid"),
                        labels = c("Male",
                                   "Female")) +
  theme(legend.position = "bottom")

ggsave("out/trends/sexcomp/timeseries_EPAD - River returns.jpg", width = 10, height = 40, limitsize = FALSE)


stat_df <- as.data.frame(logregs)

stat_df <- stat_df %>%
  filter(!is.na(Significant)) %>%
  select(Stock_Clean, Sex_Clean, N, Intercept, Slope, StdErr, Z, P, Significant)

write.csv(stat_df, "out/trends/sexcomp/stats_EPAD - River returns.csv")

params <- stat_df %>%
  filter(Sex_Clean=="Female") %>%
  mutate(Slope_pc = Slope*100) %>%
  mutate(Sig_sp=ifelse(Significant==TRUE,19,1)) %>%
  mutate(R_cl = ifelse(N<(max(stat_df$N)*0.33), "Short","Medium"),) %>%
  mutate(R_cl = ifelse(N>(max(stat_df$N)*0.66), "Long", R_cl)) %>%
  mutate(R_cl = fct_relevel(R_cl, "Short", "Medium", "Long"))

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
  ungroup()


# Scatter plots ####

params2 %>%
  mutate(Stock_Clean = fct_reorder(Stock_Clean, -Slope)) %>%
  ggplot(aes(y=Slope_pc, x=Stock_Clean, color=R_cl, fill=R_cl)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_point(size=2,
             aes(shape=Shp)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical") +
  coord_flip() +
  guides(fill="none") +
  labs(y = str_wrap("Change in female composition (River returns), percent yr-1", width = 35),
       x = "Stock",
       color = "Length of timeseries",
       shape="") +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=24, "Wild/Low enhancement"=2))  +
  scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  scale_color_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19"))

ggsave("out/trends/sexcomp/rates_EPAD - River returns.jpg", width = 10, height = 12)


# Boxplot ####

params2 %>%
  mutate(Stock = fct_reorder(Region, -Slope)) %>%
  mutate(Region = fct_relevel(Region, "WCVI", "ISC", "NC/CC")) %>%
  ggplot(aes(y=Slope, x=Region)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(size=2,
             alpha=0.5,
             aes(color=R_cl,
                 shape=Shp,
                 fill=R_cl),
             position = position_jitter(width=0.25)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_shape_manual(values = c("Enhanced (<0.05)"=19, "Enhanced"=1, 
                                "Wild/Low enhancement (<0.05)"=24, "Wild/Low enhancement"=2))  +
  scale_fill_manual(values =c("Long"="#43A047", "Medium"="#FFB300", "Short"="#E64A19")) +
  scale_color_manual(values =c("Long"="#2E7D32", "Medium"="#FFB300", "Short"="#BF360C")) +
  labs(y = "Change in female composition (River returns), percent yr-1",
       color = "Length of time series",
       shape="") +
  guides(fill="none")

ggsave("out/trends/sexcomp/rates_reg.jpg", width = 7, height = 4)

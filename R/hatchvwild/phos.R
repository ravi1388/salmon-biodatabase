# pHOS #
rm(list=ls())
library(tidyverse)

temp <- read.csv("dat/raw/pni_201105.csv")
pHOS <- temp %>%
  select(Return_Year, Population, pHOS_CWT, pHOS_Thermal)
head(pHOS)

temp <- pHOS %>%
  filter(!is.na(pHOS_CWT)) %>%
  select(Population, Return_Year) %>%
  distinct() %>%
  group_by(Population) %>%
  summarise(n_yr=n()) %>%
  filter(n_yr>4)
unique(temp$Population)

#pHOS_CWT stocks

# [1] "Ashlu Creek"                    "Atnarko River"                  "Big Qualicum River"            
# [4] "Birkenhead River"               "Bonaparte River"                "Bulkley River - upper (spring)"
# [7] "Campbell River"                 "Capilano River"                 "Cheakamus River"               
# [10] "Chilko River"                   "Chilliwack River - fall"        "Chilliwack River - summer"     
# [13] "Coldwater River"                "Conuma River"                   "Cowichan River"                
# [16] "Deadman River"                  "Dome Creek"                     "Eagle River"                   
# [19] "Harrison River"                 "Kitsumkalum River - lower"      "Kitsumkalum River - upper"     
# [22] "Lang Creek"                     "Little Campbell River"          "Little Qualicum River"         
# [25] "Mamquam River"                  "Nahmint River"                  "Nanaimo River - fall"          
# [28] "Nanaimo River - summer"         "Nicola River"                   "Nitinat River"                 
# [31] "Phillips River"                 "Porteau Cove"                   "Puntledge River - fall"        
# [34] "Puntledge River - summer"       "Quesnel River"                  "Quinsam River"                 
# [37] "Raft River"                     "Salmon River (TOMF)"            "Sarita River"                  
# [40] "Shuswap River - lower"          "Shuswap River - middle"         "Spius Creek"                   
# [43] "Stamp River - Robertson Creek"  "Stave River"                    "Stuart River"                  
# [46] "West Road (Blackwater) River"   "Yukon River"     

pHOS$Stock_Clean <- NA

pHOS$Stock_Clean[pHOS$Population=="Atnarko River"] <- "Atnarko R"
pHOS$Stock_Clean[pHOS$Population=="Big Qualicum River"] <- "Big Qualicum R"
pHOS$Stock_Clean[pHOS$Population=="Bulkley River - upper (spring)"] <- "Bulkley R Up"
pHOS$Stock_Clean[pHOS$Population=="Capilano River"] <- "Capilano R"
pHOS$Stock_Clean[pHOS$Population=="Chilliwack River - fall"] <- "Chilliwack R"
pHOS$Stock_Clean[pHOS$Population=="Conuma River"] <- "Conuma R"
pHOS$Stock_Clean[pHOS$Population=="Cowichan River"] <- "Cowichan R"
pHOS$Stock_Clean[pHOS$Population=="Eagle River"] <- "Eagle R"
pHOS$Stock_Clean[pHOS$Population=="Harrison River"] <- "Harrison R"
pHOS$Stock_Clean[pHOS$Population=="Kitsumkalum River - lower"] <- "Kitsumkalum R"
pHOS$Stock_Clean[pHOS$Population=="Kitsumkalum River - upper"] <- "Kitsumkalum R"
pHOS$Stock_Clean[pHOS$Population=="Little Qualicum River"] <- "L Qualicum R"
pHOS$Stock_Clean[pHOS$Population=="Nicola River"] <- "Nicola R"
pHOS$Stock_Clean[pHOS$Population=="Nitinat River"] <- "Nitinat R"
pHOS$Stock_Clean[pHOS$Population=="Phillips River"] <- "Phillips R"
pHOS$Stock_Clean[pHOS$Population=="Puntledge River - fall"] <- "Puntledge R"
pHOS$Stock_Clean[pHOS$Population=="Quesnel River"] <- "Quesnel R"
pHOS$Stock_Clean[pHOS$Population=="Quinsam River"] <- "Quinsam R"
pHOS$Stock_Clean[pHOS$Population=="Stamp River - Robertson Creek"] <- "Robertson Cr"
pHOS$Stock_Clean[pHOS$Population=="Salmon River (TOMF)"] <- "Salmon R/TOMF Cr"
pHOS$Stock_Clean[pHOS$Population=="Shuswap River"] <- "Shuswap R"

pHOS$Stock_Clean[pHOS$Population=="Nahmint River"] <- "Nahmint R"
pHOS$Stock_Clean[pHOS$Population=="Sarita River"] <- "Sarita R"
pHOS$Stock_Clean[pHOS$Population=="Stuart River"] <- "Stuart R"

# pHOS regression ####

# Size at age ####

# Get biodata time series
params <- read.csv("out/trends/sizeage/stats.csv")
temp <- read.csv("out/trends/sizeage/timeseries.csv")

ts <- temp %>%
  select(Stock_Clean, OceanAge, Year) %>%
  group_by(Stock_Clean, OceanAge, Year) %>%
  distinct() %>%
  ungroup() %>%
  unite("Stock_Age", Stock_Clean, OceanAge, remove = F)

stk_age <- unique(ts$Stock_Age)

i=2
ts2 <- data.frame()
for(i in 1:length(stk_age)) {
  temp <- ts %>%
    filter(Stock_Age==stk_age[i])
  temp2 <- data.frame(Stock=unique(temp$Stock_Clean),
                      OceanAge=unique(temp$OceanAge),
                      Stock_age=stk_age[i],
                      minyr=min(temp$Year),
                      maxyr=max(temp$Year))
  
  ts2 <- rbind.data.frame(ts2, temp2)
}

# pHOS (Thermal) year correction

Nechako <- data.frame(Return_Year=c(1973:2018),
                      Population="Nechako River",
                      pHOS_CWT=0,
                      pHOS_Thermal=0,
                      Stock_Clean="Nechako R")

pHOS <- pHOS %>%
  bind_rows(Nechako) %>%
  mutate(pHOS_Thermal_co=pHOS_Thermal) %>%
  mutate(pHOS_Thermal_co=ifelse(Stock_Clean=="Conuma R" & Return_Year<1999,
                                NA, pHOS_Thermal_co)) %>%
  mutate(pHOS_Thermal_co=ifelse(Stock_Clean=="Cowichan R" & Return_Year<2009,
                                NA, pHOS_Thermal_co)) %>%
  mutate(pHOS_Thermal_co=ifelse(Stock_Clean=="Nahmint R" & Return_Year<2001,
                                NA, pHOS_Thermal_co)) %>%
  mutate(pHOS_Thermal_co=ifelse(Stock_Clean=="Quinsam R" & Return_Year<2000,
                                NA, pHOS_Thermal_co)) %>%
  mutate(pHOS_Modified=pHOS_Thermal_co) %>%
  mutate(pHOS_Modified=ifelse(is.na(pHOS_Modified), pHOS_CWT, pHOS_Modified)) %>%
  filter(!is.na(pHOS_Thermal)) %>%
  gather(key="Indicator", value="pHOS", pHOS_Thermal_co, pHOS_CWT, pHOS_Modified) %>%
  mutate(Indicator = fct_relevel(Indicator, "pHOS_Thermal_co", 
                                 "pHOS_CWT", 
                                 "pHOS_Modified"))

stk <- unique(params$Stock_Clean)

pHOS %>%
  filter(Stock_Clean %in% stk) %>%
  ggplot(aes(x=Return_Year, y=pHOS, color = Indicator)) +
  geom_point() +
  geom_line() +
  facet_grid(Stock_Clean~Indicator) +
  scale_color_manual(values = c("pHOS_Thermal_co"="#F4511E",
                                "pHOS_CWT"="#7CB342",
                                "pHOS_Modified"="#26C6DA")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("out/hatchvwild/pHOS.jpg", height=10, width=9)

# Get corresponding pHOS time series and calculate mean pHOS
i=2
ind <- unique(pHOS$Indicator)

pHOS_mean <- data.frame()
for(i in 1:length(ind)) {
  for(j in 1:length(stk_age)) {
    df <- ts2 %>%
      filter(Stock_age==stk_age[j])
    df2 <- pHOS %>%
      filter(Indicator==ind[i]) %>%
      filter(!is.na(pHOS)) %>%
      filter(Stock_Clean==df$Stock) %>%
      filter(Return_Year>(df$minyr-1)) %>%
      filter(Return_Year<(df$maxyr+1))
    df3 <- df2 %>%
      group_by(Stock_Clean) %>%
      summarise(pHOS_mean = mean(pHOS))
    
    if(nrow(df2)>4) {
      df4 <- data.frame(Stock_Clean=df$Stock,
                        Age=df$OceanAge,
                        Indicator=ind[i],
                        pHOS_mean=df3$pHOS_mean)
      pHOS_mean <- rbind.data.frame(pHOS_mean, df4) 
    }
  }
}

# Add POHL deltas and plot/conduct regressions

params2 <- params %>%
  select(Slope, Stock_Clean, Age) %>%
  group_by(Stock_Clean, Age) %>%
  left_join(pHOS_mean) %>%
  filter(!is.na(pHOS_mean))

params2 %>%
  ggplot(aes(x=pHOS_mean, y=Slope, label=Stock_Clean, color = Age)) +
  geom_hline(yintercept = c(0,0), linetype = "dashed", color = "grey", size=1) +
  geom_point(size=2) +
  geom_text(vjust = 3, size = 2) +
  geom_smooth(method="lm", se=F) +
  theme_bw() +
  ylim(-10,5) +
  scale_color_manual(values = c("Ocean-4"="#F4511E","Ocean-3"="#7CB342","Ocean-2"="#26C6DA")) +
  labs(color="Ocean age",
       x = "Mean pHOS (CWT)",
       y = expression("Change in mean size, mm.yr"^-1)) +
  facet_grid(Indicator~Age)

ggsave("out/hatchvwild/pHOS_sizeatage.jpg", width = 8, height = 8)

df <- params2 %>%
  filter(Indicator=="pHOS_Thermal_co")
fit <- lm(Slope ~ pHOS_mean+Age, dat = df)
df <- params2 %>%
  filter(Indicator=="pHOS_CWT")
fit2 <- lm(Slope ~ pHOS_mean+Age, dat = df)
df <- params2 %>%
  filter(Indicator=="pHOS_Modified")
fit3 <- lm(Slope ~ pHOS_mean+Age, dat = df)
fit4 <- lm(Slope ~ Age, dat = df)
fit5 <- lm(Slope ~ 1, dat = df)

summary(fit)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

AIC(fit5, fit4, fit, fit2, fit3)

temp <- data.frame(Model=c("POHL ~ 1",
                           "POHL ~ Ocean age", "", "",
                           "POHL ~ Mean pHOS (Thermal) + Ocean age", "", "", "",
                           "POHL ~ Mean pHOS (CWT) + Ocean age", "", "", "",
                           "POHL ~ Mean pHOS (Modified) + Ocean age", "", "", ""),
                   Coefficient=c("(Intercept)",
                                 "(Intercept)", "Ocean-3", "Ocean-4",
                                 "(Intercept)", "Mean pHOS", "Ocean-3", "Ocean-4",
                                 "(Intercept)", "Mean pHOS", "Ocean-3", "Ocean-4",
                                 "(Intercept)", "Mean pHOS", "Ocean-3", "Ocean-4"),
                   Estimate=c(summary(fit5)$coefficients[1,1],
                              summary(fit4)$coefficients[1,1], 
                              summary(fit4)$coefficients[2,1],
                              summary(fit4)$coefficients[3,1],
                              summary(fit)$coefficients[1,1], 
                              summary(fit)$coefficients[2,1],
                              summary(fit)$coefficients[3,1],
                              summary(fit)$coefficients[4,1],
                              summary(fit2)$coefficients[1,1], 
                              summary(fit2)$coefficients[2,1],
                              summary(fit2)$coefficients[3,1],
                              summary(fit2)$coefficients[4,1],
                              summary(fit3)$coefficients[1,1], 
                              summary(fit3)$coefficients[2,1],
                              summary(fit3)$coefficients[3,1],
                              summary(fit3)$coefficients[4,1]),
                   SE=c(summary(fit5)$coefficients[1,2],
                        summary(fit4)$coefficients[1,2],
                        summary(fit4)$coefficients[2,2],
                        summary(fit4)$coefficients[3,2],
                        summary(fit)$coefficients[1,2],
                        summary(fit)$coefficients[2,2],
                        summary(fit)$coefficients[3,2],
                        summary(fit)$coefficients[4,2],
                        summary(fit2)$coefficients[1,2],
                        summary(fit2)$coefficients[2,2],
                        summary(fit2)$coefficients[3,2],
                        summary(fit2)$coefficients[4,2],
                        summary(fit3)$coefficients[1,2],
                        summary(fit3)$coefficients[2,2],
                        summary(fit3)$coefficients[3,2],
                        summary(fit3)$coefficients[4,2]),
                   p=c(summary(fit5)$coefficients[1,4],
                       summary(fit4)$coefficients[1,4],
                       summary(fit4)$coefficients[2,4],
                       summary(fit4)$coefficients[3,4],
                       summary(fit)$coefficients[1,4],
                       summary(fit)$coefficients[2,4],
                       summary(fit)$coefficients[3,4],
                       summary(fit)$coefficients[4,4],
                       summary(fit2)$coefficients[1,4],
                       summary(fit2)$coefficients[2,4],
                       summary(fit2)$coefficients[3,4],
                       summary(fit2)$coefficients[4,4],
                       summary(fit3)$coefficients[1,4],
                       summary(fit3)$coefficients[2,4],
                       summary(fit3)$coefficients[3,4],
                       summary(fit3)$coefficients[4,4]),
                   AIC=c(AIC(fit5),
                         AIC(fit4), "", "",
                         AIC(fit), "", "", "",
                         AIC(fit2), "", "", "",
                         AIC(fit3), "", "", ""),
                   R2=c(summary(fit5)$r.squared,
                        summary(fit4)$r.squared, "", "",
                        summary(fit)$r.squared, "", "", "",
                        summary(fit2)$r.squared, "", "", "",
                        summary(fit3)$r.squared, "", "", ""))

write.csv(temp, "out/hatchvwild/stats_pHOS.csv")

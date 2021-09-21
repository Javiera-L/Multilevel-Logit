library("tidyverse")
library("ggplot2")
library("readxl")
library("dplyr")
library("tidyr")
library("ggfortify")
library("DT")
library("reshape2")
library("knitr")
library("lubridate")
library("gtsummary")

# Load Wave 7 data
wave7 <- read_csv("WVS_Cross-National_Wave_7_csv_v1_6_2.csv")

# Select only countries surveyed in 2018
data<- wave7 %>%
  filter(wave7$A_YEAR == 2018 |
           wave7$A_YEAR == 2017 |
           wave7$A_YEAR == 2019)

# Select variables I need
myset <- c("B_COUNTRY", "B_COUNTRY_ALPHA", "Q262", "Q260","Q273", "Q279", "Q49",
           "Q58","Q59","Q60","Q61","Q62","Q63",
           "Q57",
           "Q69", "Q70", "Q71")

# Subset data
mydata <- data[myset]
# Select only complete rows (without NAs)
data_complete <- mydata[complete.cases(mydata), ]

data_complete$soc1 <- if_else(data_complete$Q58 <= 2 , 1, 0)
data_complete$soc2 <- if_else(data_complete$Q59 <= 2 , 1, 0)
data_complete$soc3 <- if_else(data_complete$Q60 <= 2 , 1, 0)
data_complete$soc4 <- if_else(data_complete$Q61 <= 2 , 1, 0)
data_complete$soc5 <- if_else(data_complete$Q62 <= 2 , 1, 0)
data_complete$soc6 <- if_else(data_complete$Q63 <= 2 , 1, 0)
data_complete$socgen <- if_else(data_complete$Q57 == 1 , 1, 0)

data_complete$Q57

# Create high trust/low trust indices
data_complete$soctrust <- if_else((rowSums(data_complete[ , c("soc1","soc2","soc3","soc4","soc5","soc6","socgen")]))>=4, 1,0)
data_complete$instrust <- if_else((rowSums(data_complete[ , c("Q69","Q70","Q71")])-2) <=6, 1,0)

# Subset final dataset
final <- dplyr::select(data_complete, 
                       -c("Q57","Q58","Q59","Q60","Q61","Q62","Q63","Q57","Q69","Q70","Q71",
                          "soc1","soc2","soc3","soc4","soc5","soc6","socgen"))

# The number of individuals per country
description <- final %>%
  group_by(B_country) %>%
  summarise(n())

# Rename the columns of my dataset
column_names <- c("country_code", "country", "age", "male", "marital_status", "employed",
                  "life_satisfaction", "soc_trust","ins_trust")

colnames(final) <- column_names

description <- final %>%
  group_by(country_code)  %>%
  summarise(n())

# Life satisfaction split between high and low values
final$high_ls <- if_else(final$life_satisfaction >= 6 ,
                       1, 0)

# Employed == 1, else 0
final$employed = if_else((final$employed == 1 |final$employed == 2 | final$employed == 3),
                         1, 0)

# Male == 1, Female == 0
final$male = if_else(final$male == 1, 1,0)

# Married/livingwith someone 
final$marital_status <- if_else(final$marital_status <= 2 ,
                               1, 0)
  
# Add age squared
final$age2 <- final$age^2


# Did not find GDP for Taiwan so remove from dataset 
final <- final %>%
  filter(final$country_code!=158)

#Import GDP values from WORLD BANK (World Development Indicators)
gdp_all <- read_csv("gdp1.csv")
length(gdp_all$`2018 [YR2018]`)
n<-dim(gdp_all)[1]
gdp<-gdp_all[1:(n-5),]

# Arrange in alphabetical order
gdp <- gdp[order(gdp$`Country Name`),]
final <- final[order(final$country),]

# Select only 2018 values
gdp_final <- gdp[c("Country Name","2018 [YR2018]")]
# Add country code
gdp_final$country_code <- description$country_code
# Rename cols
colnames(gdp_final) <- c("country_name", "gdp", "country_code")

# Merge with final dataset
final <- left_join(final,gdp_final)

# Agg trust
agg_trust <-aggregate(final[, c("ins_trust", "soc_trust")], 
                      list(final$country_code), 
                      mean)
agg_trust$country_inst <- if_else(agg_trust$ins_trust >= 0.5  , 1, 0)
agg_trust$country_soct <- if_else(agg_trust$soc_trust >= 0.5  , 1, 0)
agg_trust_final <- agg_trust[c("Group.1", "country_inst", "country_soct")]
colnames(agg_trust_final) <- c("country_code", "country_inst", "country_soct")
final <- left_join(final,agg_trust_final)

# Summary stats
col_select <- c("country_name", "age", "male",
                "marital_status", "employed", "soc_trust",
                "ins_trust", "high_ls", "country_soct")
summary_stats_cols <- c("Country", "Age", "Sex",
                        "Marital Status", "Employment Status",
                        "Social Trust", "Institutional Trust", "Life Satisfaction", 
                        "Country Social Trust")

summary_stats <- final[col_select]
colnames(summary_stats) <- summary_stats_cols
summary_stats$Sex <- if_else(summary_stats$Sex ==1, "Male", "Female")
summary_stats$`Marital Status` <- if_else(summary_stats$`Marital Status` ==1, 
                                          "Married", "Not married")
summary_stats$`Employment Status` <- if_else(summary_stats$`Employment Status` ==1, 
                                             "Employed", "Not employed")
summary_stats$`Social Trust` <- if_else(summary_stats$`Social Trust` ==1, 
                                        "High", "Low")
summary_stats$`Institutional Trust` <- if_else(summary_stats$`Institutional Trust` ==1, 
                                        "High", "Low")
summary_stats$`Country Social Trust` <- if_else(summary_stats$`Country Social Trust` ==1, 
                                        "High", "Low")
summary_stats$`Life Satisfaction` <- if_else(summary_stats$`Life Satisfaction` ==1, 
                                        "High", "Low")


testsummary <- summary_stats %>%
  tbl_summary(
    #by = country_code,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}  ({p}%)"),
    #digits = all_continuous() ~ 2,
    #label = grade ~ "Tumor Grade",
    #missing_text = "(Missing)"
  )


# Load lmer library
library(lme4)
library(lmer)
# Null model
nullmodel <- glmer(high_ls ~ 1+ (1 | country_name), 
                  data=final, 
                  family = binomial(link="logit"))

# adding level 1 predictors

m1 <- glmer(high_ls ~ male+ (1 | country_name), 
                   data=final, 
                   family = binomial(link="logit"))

# adding age 
final$age_scaled <- scale(final$age)
final$age2_scaled <- scale(final$age2)

m2 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+ (1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# marital status
m3 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status +(1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# employment status

m4 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status + +employed +(1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# soc trust

m5 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status + +employed +
              soc_trust+(1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# ins_trust
m6 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status + +employed +
              ins_trust+(1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# both trusts

m7 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status + +employed +
              ins_trust+soc_trust+(1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# gdp
final$gdp_scaled <- scale(final$gdp)

m8 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status + +employed +
              ins_trust+soc_trust+
              +gdp_scaled+(1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# with interaction term
m9 <- glmer(high_ls ~ male+ age2_scaled+age_scaled+
              marital_status + +employed + 
              ins_trust+soc_trust*country_soct+
              +gdp_scaled+ (1 | country_name), 
            data=final, 
            family = binomial(link="logit"))

# AIC(nullmodel, m1, m2,m3,m4,m5,m6,m7,m8,m9)

# plot country rank
library(merTools)
reEX <- REsim(m9)
plotREsim(reEX,oddsRatio = FALSE, labs=TRUE)

#### table of models
library(stargazer)

library(gtsummary)
library(tidyverse)
remotes::install_github("rstudio/gt")

stargazer(nullmodel, m1,m2,m3,m4,m5,m6,m7, m8, m9,
          title = 'Model Results',
          type = 'html',
          out = 'allmodelssummary.html')


# finding the log odds
tab <- c(fixef(m9))
# odds ratios
exp(tab)


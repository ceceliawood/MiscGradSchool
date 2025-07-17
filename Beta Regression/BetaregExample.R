#betareg() example


library(tidyverse)
library(ggplot2)
library(stats)
library(readxl)
library(performance)
library(betareg)
library(broom)
library(generics)
library(lmtest)

# pull in data from github

frame1 <- read_csv('https://raw.githubusercontent.com/ceceliawood/MiscGradSchool/refs/heads/main/Beta%20Regression/Data%20Mining_end.csv') %>% 
  mutate(Gt_pct = Gt_pct/100, # turn percents into decimals
         Lp_pct = Lp_pct/100,
         Fh_pct = Fh_pct/100,
         Mt_pct = Mt_pct/100) %>% 
  rename('EV1' = 'Buffer1',        # renaming variables for ease of use
         'EV2' = 'CounterIon1',    # EV = explanatory variable
         'EV3' = 'pH',             # RV = response variable
         'EV4' = 'Fe2Fh_mMgL',
         'EV5' = 'Time_hr',
         'EV6' = 'CFe_molmol',
         'EV7' = 'Al_molpercent',
         'RV1' = 'Fh_pct',
         'RV2' = 'Gt_pct',
         'RV3' = 'Lp_pct',
         'RV4' = 'Mt_pct') %>%
  select(contains('V')) %>% 
  #slight transformations to meet assumptions of beta reg
  mutate(across(contains('RV'), ~if_else(. == 0, 0.0001, .))) %>% 
  mutate(across(contains('RV'), ~if_else(. == 1, 0.9999, .)))

plot(frame1)

# EVs are the experimental conditions that made a mixed sample
# They are a mix of continuous variables (Time, pH, iron ratios) and
# discrete variables (Buffer 1, 2, or 3; CounterIon 1 or 2)

# RVs are the proportion of one mineral phase in a mixed sample
# If RV1 = 0.75, then 75% of the mixed sample is phase 1
# Thus, RVs range from 0-1 and include both 0 and 1

RV2_logit <- betareg(RV2 ~ EV2 + EV3 + EV4 + EV5 + EV6 + EV6, data = frame1,
                    link = 'logit')
summary(RV2_logit)
plot(RV2_logit)



RV2_logit2 <- betareg(RV2 ~ EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7, data = frame1,
                     link = 'logit')
summary(RV2_logit2)
plot(RV2_logit2)


RV2_logit_table2 <- RV2_logit2 %>% 
  tidy() %>% 
  mutate(plogis_est = c(plogis(estimate[1]),
                        plogis(estimate[1] + estimate[2]),
                        plogis(estimate[1] + estimate[3]),
                        plogis(estimate[1] + estimate[4]),
                        plogis(estimate[1] + estimate[5]),
                        plogis(estimate[1] + estimate[6]),
                        plogis(estimate[1] + estimate[7]),
                        plogis(estimate[1] + estimate[8]),
                        plogis(estimate[1] + estimate[9]),
                        plogis(estimate[1] + estimate[10])))


  
       
lrtest(RV1_logit, RV1_logit2)


RV1_logit <- betareg(RV1 ~ EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7, data = frame1,
                      link = 'logit')
summary(RV1_logit)
plot(RV1_logit)


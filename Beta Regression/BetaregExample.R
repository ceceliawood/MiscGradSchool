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
library(tinytable)
library(parameters)
library(insight)

# pull in data from github

frame1 <- read_csv('https://raw.githubusercontent.com/ceceliawood/MiscGradSchool/637e604802d3910581a99e7f69a91c39eaafc347/Beta%20Regression/Data%20Mining_end.csv') %>% 
  mutate(Gt_pct = Gt_pct/100, # turn percents into decimals
         Lp_pct = Lp_pct/100,
         Fh_pct = Fh_pct/100,
         Mt_pct = Mt_pct/100) %>% 
  rename('EV1' = 'Buffer1',        # renaming variables for ease of use
         'EV2' = 'CounterIon1',    # EV = explanatory variable
         'EV3' = 'pH',             # RV = response variable
         'EV4' = 'Fe2Fh_mMgL',
         'EV5' = 'Time_hr',        #note that in Buffer1, 'A' = unbuffered
         'EV6' = 'CFe_molmol',
         'EV7' = 'Al_molpercent',
         'EV8' = 'SiFe_molmol',
         'RV1' = 'Fh_pct',
         'RV2' = 'Gt_pct',
         'RV3' = 'Lp_pct',
         'RV4' = 'Mt_pct') %>%
  select(contains('V')) %>% 
  #slight transformations to meet assumptions of beta reg
  mutate(across(contains('RV'), ~if_else(. == 0, 0.0001, .))) %>% 
  mutate(across(contains('RV'), ~if_else(. == 1, 0.9999, .))) %>%
  mutate(EV6 = if_else(EV6 == 0, 0.001, EV6)) %>% 
  mutate(EV7 = if_else(EV7 == 0, 0.001, EV7)) %>% 
  mutate(EV8 = if_else(EV8 == 0, 0.001, EV8))

plot(frame1)

frame1_filt <- frame1 %>% 
  filter(EV5 < 2000) #testing getting rid of highest time point

frame2 <- frame1 %>% 
  filter(EV1 != 'A')

# EVs are the experimental conditions that made a mixed sample
# They are a mix of continuous variables (Time, pH, iron ratios) and
# discrete variables (Buffer 1, 2, or 3; CounterIon 1 or 2)

# RVs are the proportion of one mineral phase in a mixed sample
# If RV1 = 0.75, then 75% of the mixed sample is phase 1
# Thus, RVs range from 0-1 and include both 0 and 1

RV2_logit <- betareg(RV2 ~ EV1 + EV2 + log(EV4) + log(EV5) + log(EV6) + log(EV7) + EV8 | EV5, data = frame1,
                    link = 'logit')
summary(RV2_logit)
plot(RV2_logit)



RV2_logit2 <- betareg(RV2 ~ EV1 + EV2 + log(EV4) + log(EV5) + log(EV6) + log(EV7) + EV8 | log(EV5), data = frame1,
                     link = 'logit')
summary(RV2_logit2)
plot(RV2_logit2)


RV2_logit_table2 <- RV2_logit2 %>% 
  tidy() %>% 
  mutate(Odds = signif(exp(estimate), digits = 3),
         p.value = signif(p.value, digits = 3),
         significant = if_else(p.value < 0.05, 'Y', 'N'))


  
       
lrtest(RV2_logit2, RV2_logit)





RV1_logit1 <- betareg(RV1 ~ EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7 + EV8 + EV4*EV5, data = frame1,
                      link = 'logit')
summary(RV1_logit1)
plot(RV1_logit1)


RV3_logit1 <- betareg(RV3 ~ EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7 + EV8 + EV4*EV5, data = frame1,
                      link = 'logit')
summary(RV3_logit1)
plot(RV3_logit1)


RV4_logit1 <- betareg(RV4 ~ EV1 + EV2 + EV3 + EV4 + EV5 + EV6 + EV7 + EV8 + EV4*EV5, data = frame1,
                      link = 'logit')
summary(RV4_logit1)
plot(RV4_logit1)


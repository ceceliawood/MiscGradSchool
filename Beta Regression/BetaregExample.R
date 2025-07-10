#betareg() example


library(tidyverse)
library(ggplot2)
library(stats)
library(readxl)
library(performance)
library(betareg)
library(broom)
library(generics)

# pull in data from github

frame1 <- read_csv('https://raw.githubusercontent.com/ceceliawood/MiscGradSchool/refs/heads/main/Beta%20Regression/DataMining.csv') %>% 
  mutate(Gt_pct = Gt_pct/100, # turn percents into decimals
         Lp_pct = Lp_pct/100,
         Fh_pct = Fh_pct/100,
         Mt_pct = Mt_pct/100) %>% 
  rename('EV1' = 'Buffer1',        # renaming variables for ease of use
         'EV2' = 'CounterIon1',    # EV = explanatory variable
         'EV3' = 'pH',             # RV = response variable
         'EV4' = 'Fe2Fh_mMg',
         'EV5' = 'Time_hr',
         'RV1' = 'Fh_pct',
         'RV2' = 'Gt_pct',
         'RV3' = 'Lp_pct',
         'RV4' = 'Mt_pct') %>%
select(contains('V'))

plot(frame1)

# EVs are the experimental conditions that made a mixed sample
# They are a mix of continuous variables (Time, pH, iron ratios) and
# discrete variables (Buffer 1, 2, or 3; CounterIon 1 or 2)

# RVs are the proportion of one mineral phase in a mixed sample
# If RV1 = 0.75, then 75% of the mixed sample is phase 1
# Thus, RVs range from 0-1 and include both 0 and 1

RV2_logit <- betareg(RV2 ~ EV1 + EV2 + EV3 + EV4 + EV5 | EV5, data = frame1,
                    link = 'logit')
summary(RV2_logit)
plot(RV2_logit)

beta_mu_intercept <- RV2_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "(Intercept)") %>% 
  pull(estimate)

beta_mu_EV1MES <- RV2_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "EV1MES") %>% 
  pull(estimate)

beta_mu_EV1PIPES <- RV2_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "EV1PIPES") %>% 
  pull(estimate)

beta_mu_EV2sulfate <- RV2_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "EV2sulfate") %>% 
  pull(estimate)

beta_mu_EV3 <- Gt_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "EV3") %>% 
  pull(estimate)

beta_mu_EV4 <- Gt_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "EV4") %>% 
  pull(estimate)

beta_mu_EV5 <- Gt_logit %>% 
  tidy() %>% 
  filter(component == "mu", term == "EV5") %>% 
  pull(estimate)

RV2_logit_table <- tidy(RV2_logit) %>% 
  mutate(Probability = c(plogis(beta_mu_intercept),
                         plogis(beta_mu_intercept + beta_mu_EV1MES) - plogis(beta_mu_intercept),
                         plogis(beta_mu_intercept + beta_mu_EV1PIPES) - plogis(beta_mu_intercept),
                         plogis(beta_mu_intercept + beta_mu_EV2sulfate) - plogis(beta_mu_intercept),
                         plogis(beta_mu_intercept + beta_mu_EV3) - plogis(beta_mu_intercept),
                         plogis(beta_mu_intercept + beta_mu_EV4) - plogis(beta_mu_intercept),
                         plogis(beta_mu_intercept + beta_mu_EV5) - plogis(beta_mu_intercept),
                         NA,
                         NA,
                         NA))

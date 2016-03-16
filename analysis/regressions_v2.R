# ---------------------------------------------------------------------------- #
# Preliminary Analysis
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(DataCombine)
library(arm)
#library(Zelig)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

# Set as factors
main$country <- as.factor(main$country)
main$year <- as.factor(main$year)
main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- as.factor(main$executive_election_4qt)

FindDups(main, c('country', 'year_quarter'))

# Only democracies ------
dem <- main %>% filter(polity2 > 5)

# dem_bis_sub <- dem %>% DropNA('bis_housing_change')

# Simple logistic regressions tightening --------
t1 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   finstress_qt_mean + bis_housing_change +
                   factor(country) + factor(quarter)
               , data = dem, family = binomial(link = 'logit'))

t2 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   `_1_gini_market` +
                   factor(country) + factor(quarter)
               , data = dem, family = binomial(link = 'logit'))

t3 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   factor(country) + factor(quarter)
          , data = dem, family = binomial(link = 'logit'))

t4 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi + execrlc + executive_election_4qt +
                   factor(country) + factor(quarter)
               , data = dem, family = binomial(link = 'logit'))

t5 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation + 
                   cbi + fiscal_trans_gfs +
                   factor(country) + factor(quarter)
               , data = dem, family = binomial(link = 'logit'))

t6 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi + domestic_credit_change +
                   factor(country) + factor(quarter)
               , data = dem, family = binomial(link = 'logit'))

library(texreg)
screenreg(list(t1, t2, t3, t4, t5, t6),
       omit.coef = 'factor')

# Simple logistic regressions loosening -------
l1 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   finstress_qt_mean + bis_housing_change +
                   factor(country) + factor(year) + factor(quarter)
          , data = dem, family = binomial(link = 'logit'))

l2 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   `_1_gini_market` + 
                   factor(country) + factor(year) + factor(quarter)
          , data = dem, family = binomial(link = 'logit'))

l3 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   fiscal_trans_gfs + 
                   executive_election_4qt + cbi +
              factor(country) + factor(quarter)
          , data = dem, family = binomial(link = 'logit'))

l4 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + executive_election_4qt + execrlc +
              country + year + quarter
          , data = dem, family = binomial(link = 'logit'))

l5 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs +
              executive_election_4qt + domestic_credit_change +
              country + year + quarter
          , data = dem, family = binomial(link = 'logit'))

l6 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs +
              executive_election_4qt + gdp_growth +
              country + year + quarter
          , data = dem, family = binomial(link = 'logit'))

l7 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs +
              executive_election_4qt + inflation +
              country + year + quarter
          , data = dem, family = binomial(link = 'logit'))

l8 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + finstress_qt_mean +
              executive_election_4qt +
              country + year + quarter
          , data = dem, family = binomial(link = 'logit'))

# Rare logistic Regression
#r_t3 <- zelig(any_tighten ~ finstress_qt_mean + cbi +
#              country + year
#          , data = dem, model = 'relogit', cite = F)


#r_l3 <- zelig(any_loosen ~ fiscal_trans_gfs + cbi +
#              country + year
#          , data = dem, model = 'relogit', cite = F)
